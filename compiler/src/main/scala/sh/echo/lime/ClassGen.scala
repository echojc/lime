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
      m.visitTypeInsn(CHECKCAST, ot)
      m.visitMethodInsn(INVOKEVIRTUAL, ot, em, s"()$tpe", false)
    }
    def box(tpe: String): Unit = {
      val (ot, _) = paramsFor(tpe)
      m.visitMethodInsn(INVOKESTATIC, ot, "valueOf", s"($tpe)L$ot;", false)
    }
  }

  case class Ins(run: MethodVisitor ⇒ Unit, tpe: String)

  case class UnitContext(
    name: String,
    knownFuns: List[String]
  )

  case class FunContext(
    name: String,
    args: List[String],
    unitCtx: UnitContext
  ) {
    // stack offset for local variables
    val varOffset: Int = args.size
  }
}

class ClassGen {
  import ClassGen._
  import Ops._

  def compileUnit(unit: String, expr: List[Expr]): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_7, ACC_PUBLIC + ACC_SUPER, unit, null, "java/lang/Object", null)

    // collect all user defined functions
    val (funs, freeExprs) = expr partition {
      case Exprs(Atom("def") :: Atom(name) :: Exprs(args) :: body :: Nil) ⇒
        true
      case _ ⇒
        false
    }

    val knownFuns: List[String] = funs map { case Exprs(_ :: Atom(name) :: _) ⇒ name.toString }
    val unitCtx = UnitContext(unit, knownFuns)

    funs foreach {
      case Exprs(Atom("def") :: Atom(name) :: Exprs(args) :: expr :: Nil) ⇒
        require(args forall (_.isInstanceOf[Atom]))
        val argNames = args.map(_.asInstanceOf[Atom].value.toString)
        val funCtx = FunContext(name.toString, argNames, unitCtx)
        compileFun(cw, expr, funCtx)
      case expr @ _ ⇒
        throw new RuntimeException(s"don't know how to compile expr:\n$expr")
    }

    // only generate main if we need to
    if (!freeExprs.isEmpty)
      compileMain(cw, freeExprs, unitCtx)

    cw.visitEnd()
    cw.toByteArray()
  }

  def compileFun(cw: ClassWriter, expr: Expr, funCtx: FunContext): Unit = {
    val desc = s"(${(funCtx.args map (_ ⇒ O)).mkString})$O"
    val m = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, funCtx.name, desc, null, null)
    m.visitCode()
    val ins = compileExpr(m, expr, funCtx)
    ins.run(m)
    if (ins.tpe != "A")
      m.box(ins.tpe)
    m.visitInsn(ARETURN)
    m.visitMaxs(0, 0)
    m.visitEnd()
  }

  def compileMain(cw: ClassWriter, exprs: List[Expr], unitCtx: UnitContext): Unit = {
    // collect free exprs into a delegate main function
    val m = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "(Llime/List;)V", null, null)
    m.visitCode()

    // compile each expr
    val funCtx = FunContext("main", List("args"), unitCtx)
    exprs foreach { expr ⇒
      // TODO this will leave stuff on the stack...
      compileExpr(m, expr, funCtx).run(m)
    }

    m.visitInsn(RETURN)
    m.visitMaxs(0, 0)
    m.visitEnd()

    // generate java main function that calls delegate
    val m2 = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    m2.visitCode()
    m2.visitVarInsn(ALOAD, 0)
    m2.visitMethodInsn(INVOKESTATIC, "lime/Cons", "fromArray", "([Ljava/lang/Object;)Llime/List;", false)
    m2.visitMethodInsn(INVOKESTATIC, unitCtx.name, "main", "(Llime/List;)V", false)
    m2.visitInsn(RETURN)
    m2.visitMaxs(0, 0)
    m.visitEnd()
  }

  def compileExpr(m: MethodVisitor, expr: Expr, funCtx: FunContext): Ins = {
    expr match {
      case Atom(n: Long) ⇒
        Ins(_.visitLdcInsn(n), "J")
      case Atom(s: String) ⇒
        require(funCtx.args.contains(s))
        Ins(_.visitVarInsn(ALOAD, funCtx.args.indexOf(s)), "A")
      case Exprs(Atom(fun: String) :: exprs) ⇒
        val args = exprs map (expr ⇒ compileExpr(m, expr, funCtx))
        compileFunCall(m, fun, args, funCtx)
      case _ ⇒
        throw new RuntimeException(s"don't know how to compile expr:\n$expr")
    }
  }

  def compileFunCall(m: MethodVisitor, fun: String, args: List[Ins], funCtx: FunContext): Ins = {
    fun match {
      case "+" ⇒
        Ins(m ⇒ {
          args foreach { i ⇒
            i.run(m)
            if (i.tpe == "A") m.unbox("J")
          }
          m.visitInsn(LADD)
        }, "J")
      case "list" ⇒
        Ins(m ⇒ {
          // push each value onto the stack
          args foreach { i ⇒
            m.visitTypeInsn(NEW, "lime/Cons")
            m.visitInsn(DUP)
            i.run(m)
            if (i.tpe != "A") m.box(i.tpe)
          }
          // push nil
          m.visitMethodInsn(INVOKESTATIC, "lime/Nil", "get", "()Llime/List;", false)
          // cons each one
          args foreach { _ ⇒
            m.visitMethodInsn(INVOKESPECIAL, "lime/Cons", "<init>", "(Ljava/lang/Object;Llime/List;)V", false)
          }
        }, "A")
      case "cons" ⇒
        require(args.size == 2)
        Ins(m ⇒ {
          m.visitTypeInsn(NEW, "lime/Cons")
          m.visitInsn(DUP)
          val fst :: snd :: _ = args
          fst.run(m)
          if (fst.tpe != "A") m.box(fst.tpe)
          snd.run(m)
          m.visitTypeInsn(CHECKCAST, "lime/List")
          m.visitMethodInsn(INVOKESPECIAL, "lime/Cons", "<init>", "(Ljava/lang/Object;Llime/List;)V", false)
        }, "A")
      case "car" ⇒
        require(args.size == 1)
        Ins(m ⇒ {
          val l = args.head
          l.run(m)
          m.visitTypeInsn(CHECKCAST, "lime/List")
          m.visitMethodInsn(INVOKEINTERFACE, "lime/List", "head", "()Ljava/lang/Object;", true)
        }, "A")
      case "cdr" ⇒
        require(args.size == 1)
        Ins(m ⇒ {
          val l = args.head
          l.run(m)
          m.visitTypeInsn(CHECKCAST, "lime/List")
          m.visitMethodInsn(INVOKEINTERFACE, "lime/List", "tail", "()Llime/List;", true)
        }, "A")
      case "if" ⇒
        require(args.size == 3)
        val e :: t :: f :: _ = args
        val sameType = t.tpe == f.tpe
        Ins(m ⇒ {
          val (l0, l1) = (new Label(), new Label())
          e.run(m)
          if (e.tpe != "J") m.unbox("J")
          m.visitInsn(LCONST_0)
          m.visitInsn(LCMP) // check if _false_
          m.visitJumpInsn(IFEQ, l0) // if _false_ jump
          t.run(m)
          if (!sameType && t.tpe != "A") m.box(t.tpe)
          m.visitJumpInsn(GOTO, l1)
          m.visitLabel(l0)
          f.run(m)
          if (!sameType && f.tpe != "A") m.box(f.tpe)
          m.visitLabel(l1)
        }, if (sameType) t.tpe else "A")
      case "str" ⇒
        Ins(m ⇒ visitStringBuilder(m, args), "A")
      case "put" ⇒
        Ins(m ⇒ {
          m.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
          visitStringBuilder(m, args)
          m.visitInsn(DUP)
          m.visitVarInsn(ASTORE, funCtx.varOffset)
          m.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
          m.visitVarInsn(ALOAD, funCtx.varOffset)
        }, "A")
      case userFun @ _ if funCtx.unitCtx.knownFuns contains userFun ⇒
        Ins(m ⇒ {
          args foreach { i ⇒
            i.run(m)
            if (i.tpe != "A") m.box(i.tpe)
          }
          m.visitMethodInsn(INVOKESTATIC, "$default", userFun, "()Ljava/lang/Object;", false)
        }, "A")
      case unknownFun @ _ ⇒
        throw new UnknownFunctionException(unknownFun)
    }
  }

  private def visitStringBuilder(m: MethodVisitor, args: List[Ins]): Unit = {
    m.visitTypeInsn(NEW, "java/lang/StringBuilder");
    m.visitInsn(DUP);
    m.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
    args foreach { i ⇒
      i.run(m)
      if (i.tpe != "A") m.box(i.tpe)
      m.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false)
    }
    m.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false)
  }
}
