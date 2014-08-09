package sh.echo.lime

import org.objectweb.asm._

object ClassGen {
  import Ops._
  val O = "Ljava/lang/Object;"

  def paramsFor(tpe: String): (String, String) = tpe match {
    case "J" ⇒ ("java/lang/Long", "longValue")
  }

  implicit class BoxingInsn(m: MethodVisitor) {
    def unbox(tpe: String): Unit = {
      val (ot, em) = paramsFor(tpe)
      m.visitTypeInsn(CHECKCAST, s"L$ot;")
      m.visitMethodInsn(INVOKEVIRTUAL, ot, em, s"()$tpe", false)
    }
    def box(tpe: String): Unit = {
      val (ot, _) = paramsFor(tpe)
      m.visitMethodInsn(INVOKESTATIC, ot, "valueOf", s"($tpe)L$ot;", false)
    }
  }

  case class Ins(run: MethodVisitor ⇒ Unit, tpe: String)
}

class ClassGen {
  import ClassGen._
  import Ops._

  def compileUnit(name: String, expr: List[Expr]): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_7, ACC_PUBLIC + ACC_SUPER, name, null, "java/lang/Object", null)

    // collect all user defined functions
    val knownFuns = expr collect {
      case Exprs(Atom("def") :: Atom(name) :: Exprs(args) :: body :: Nil) ⇒
        name.toString
    }

    expr foreach {
      case Exprs(Atom("def") :: Atom(name) :: Exprs(args) :: body :: Nil) ⇒
        require(args forall (_.isInstanceOf[Atom]))
        val argNames = args.map(_.asInstanceOf[Atom].value.toString)
        compileMethod(cw, name.toString, argNames, body, knownFuns)
      case expr @ _ ⇒
        throw new RuntimeException(s"don't know how to compile expr:\n$expr")
    }

    cw.visitEnd()
    cw.toByteArray()
  }

  def compileMethod(cw: ClassWriter, name: String, args: List[String], expr: Expr, knownFuns: List[String]): Unit = {
    val desc = s"(${(args map (_ ⇒ O)).mkString})$O"
    val m = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, name, desc, null, null)
    m.visitCode()
    val ins = compileExpr(m, expr, args, knownFuns)
    ins.run(m)
    if (ins.tpe != "A")
      m.box(ins.tpe)
    m.visitInsn(ARETURN)
    m.visitMaxs(0, 0)
    m.visitEnd()
  }

  def compileExpr(m: MethodVisitor, expr: Expr, scopeArgs: List[String], knownFuns: List[String]): Ins = {
    expr match {
      case Atom(n: Long) ⇒
        Ins(_.visitLdcInsn(n), "J")
      case Atom(s: String) ⇒
        require(scopeArgs.contains(s))
        Ins(_.visitVarInsn(ALOAD, scopeArgs.indexOf(s)), "A")
      case Exprs(Atom(fun: String) :: rest) ⇒
        val insArgs = rest map (compileExpr(m, _, scopeArgs, knownFuns))
        compileFunCall(m, fun, insArgs, knownFuns)
      case _ ⇒
        throw new RuntimeException(s"don't know how to compile expr:\n$expr")
    }
  }

  def compileFunCall(m: MethodVisitor, fun: String, insArgs: List[Ins], knownFuns: List[String]): Ins = {
    fun match {
      case "+" ⇒
        Ins(m ⇒ {
          insArgs foreach { i ⇒
            i.run(m)
            if (i.tpe == "A") m.unbox("J")
          }
          m.visitInsn(LADD)
        }, "J")
      case "list" ⇒
        Ins(m ⇒ {
          // push each value onto the stack
          insArgs foreach { i ⇒
            m.visitTypeInsn(NEW, "lime/Cons")
            m.visitInsn(DUP)
            i.run(m)
            if (i.tpe != "A") m.box(i.tpe)
          }
          // push nil
          m.visitMethodInsn(INVOKESTATIC, "lime/Nil", "get", "()Llime/List;", false)
          // cons each one
          insArgs foreach { _ ⇒
            m.visitMethodInsn(INVOKESPECIAL, "lime/Cons", "<init>", "(Ljava/lang/Object;Llime/List;)V", false)
          }
        }, "A")
      case "cons" ⇒
        require(insArgs.size == 2)
        Ins(m ⇒ {
          m.visitTypeInsn(NEW, "lime/Cons")
          m.visitInsn(DUP)
          val fst :: snd :: _ = insArgs
          fst.run(m)
          if (fst.tpe != "A") m.box(fst.tpe)
          snd.run(m)
          m.visitTypeInsn(CHECKCAST, "lime/List")
          m.visitMethodInsn(INVOKESPECIAL, "lime/Cons", "<init>", "(Ljava/lang/Object;Llime/List;)V", false)
        }, "A")
      case "car" ⇒
        require(insArgs.size == 1)
        Ins(m ⇒ {
          val l = insArgs.head
          l.run(m)
          m.visitTypeInsn(CHECKCAST, "lime/List")
          m.visitMethodInsn(INVOKEINTERFACE, "lime/List", "head", "()Ljava/lang/Object;", true)
        }, "A")
      case "cdr" ⇒
        require(insArgs.size == 1)
        Ins(m ⇒ {
          val l = insArgs.head
          l.run(m)
          m.visitTypeInsn(CHECKCAST, "lime/List")
          m.visitMethodInsn(INVOKEINTERFACE, "lime/List", "tail", "()Llime/List;", true)
        }, "A")
      case "if" ⇒
        require(insArgs.size == 3)
        val e :: t :: f :: _ = insArgs
        val sameType = t.tpe == f.tpe
        Ins(m ⇒ {
          val (l0, l1) = (new Label(), new Label())
          e.run(m)
          m.visitJumpInsn(IFNE, l0)
          t.run(m)
          if (!sameType && t.tpe != "A") m.box(t.tpe)
          m.visitJumpInsn(GOTO, l1)
          m.visitLabel(l0)
          f.run(m)
          if (!sameType && f.tpe != "A") m.box(f.tpe)
          m.visitLabel(l1)
        }, if (sameType) t.tpe else "A")
      case userFun @ _ if knownFuns contains userFun ⇒
        Ins(m ⇒ {
          insArgs foreach { i ⇒
            i.run(m)
            if (i.tpe != "A") m.box(i.tpe)
          }
          m.visitMethodInsn(INVOKESTATIC, "$default", userFun, "()Ljava/lang/Object;", false)
        }, "A")
      case unknownFun @ _ ⇒
        throw new UnknownFunctionException(unknownFun)
    }
  }
}
