package sh.echo.lime

import org.objectweb.asm._

object ClassGen {
  import Ops._
  val O = "Ljava/lang/Object;"
  val E = Array[String]("java/lang/Exception")

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
    knownFuns: Map[String, FunDef]
  )

  case class FunDef(
    name: String,
    args: List[String]
  ) {
    val argc = args.size
    val desc = s"(${Seq.fill(argc)(O).mkString})$O"
  }

  case class FunContext(
    funDef: FunDef,
    unitCtx: UnitContext
  ) {
    // stack offset for local variables
    val varOffset: Int = funDef.args.size
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
      case Exprs(Ident("def") :: Ident(name) :: Exprs(args) :: body :: Nil) ⇒
        true
      case _ ⇒
        false
    }

    val knownFuns: Map[String, FunDef] =
      funs.map { case Exprs(_ :: Ident(name) :: Exprs(args) :: _) ⇒
        require(args forall (_.isInstanceOf[Ident]))
        val nameStr = name.toString
        val argNames = args.map(_.asInstanceOf[Ident].name)
        nameStr → FunDef(nameStr, argNames)
      }.toMap
    val unitCtx = UnitContext(unit, knownFuns)

    funs foreach {
      case Exprs(Ident("def") :: Ident(name) :: Exprs(args) :: expr :: Nil) ⇒
        require(knownFuns contains name.toString)
        val funCtx = FunContext(knownFuns(name.toString), unitCtx)
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
    val m = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, funCtx.funDef.name, funCtx.funDef.desc, null, E)
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
    val m = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "(Llime/List;)V", null, E)
    m.visitCode()

    // compile each expr
    val funCtx = FunContext(FunDef("main", List("args")), unitCtx)
    exprs foreach { expr ⇒
      // TODO this will leave stuff on the stack...
      compileExpr(m, expr, funCtx).run(m)
    }

    m.visitInsn(RETURN)
    m.visitMaxs(0, 0)
    m.visitEnd()

    // generate java main function that calls delegate
    val m2 = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, E)
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
      case NumberConst(n) ⇒
        Ins(_.visitLdcInsn(n), "J")
      case StringConst(s) ⇒
        Ins(_.visitLdcInsn(s), "A")
      case Ident(name) if funCtx.funDef.args contains name ⇒
        Ins(_.visitVarInsn(ALOAD, funCtx.funDef.args.indexOf(name)), "A")
      case Ident(name) if funCtx.unitCtx.knownFuns contains name ⇒
        Ins(m ⇒ {
          m.visitLdcInsn(Type.getType(s"L${funCtx.unitCtx.name};"))
          m.visitLdcInsn(name)
          val funDef = funCtx.unitCtx.knownFuns(name)
          // begin varargs
          m.visitLdcInsn(funDef.argc)
          m.visitTypeInsn(ANEWARRAY, "java/lang/Class")
          (0 until funDef.argc) foreach { index ⇒
            m.visitInsn(DUP)
            m.visitLdcInsn(index)
            m.visitLdcInsn(Type.getType(O))
            m.visitInsn(AASTORE)
          }
          // end varargs
          m.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getMethod",
            "(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;", false)
        }, "A")
      case Exprs(Ident(fun) :: exprs) ⇒
        val args = exprs map (expr ⇒ compileExpr(m, expr, funCtx))
        compileFunCall(m, fun, args, funCtx)
      case _ ⇒
        throw new RuntimeException(s"don't know how to compile expr:\n$expr")
    }
  }

  def compileFunCall(m: MethodVisitor, fun: String, args: List[Ins], funCtx: FunContext): Ins = {
    fun match {
      case argFun @ _ if funCtx.funDef.args contains argFun ⇒
        Ins(m ⇒ {
          m.visitVarInsn(ALOAD, funCtx.funDef.args.indexOf(argFun))
          m.visitTypeInsn(CHECKCAST, "java/lang/reflect/Method")
          m.visitInsn(ACONST_NULL)
          // begin varargs
          m.visitLdcInsn(args.size)
          m.visitTypeInsn(ANEWARRAY, "java/lang/Object")
          args.zipWithIndex foreach { case (i, index) ⇒
            m.visitInsn(DUP)
            m.visitLdcInsn(index)
            i.run(m)
            if (i.tpe != "A") m.box(i.tpe)
            m.visitInsn(AASTORE)
          }
          // end varargs
          m.visitMethodInsn(INVOKEVIRTUAL, "java/lang/reflect/Method", "invoke", s"($O[$O)$O", false)
        }, "A")
      case userFun @ _ if funCtx.unitCtx.knownFuns contains userFun ⇒
        Ins(m ⇒ {
          args foreach { i ⇒
            i.run(m)
            if (i.tpe != "A") m.box(i.tpe)
          }
          val userFunDef = funCtx.unitCtx.knownFuns(userFun)
          m.visitMethodInsn(INVOKESTATIC, funCtx.unitCtx.name, userFunDef.name, userFunDef.desc, false)
        }, "A")
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
      case "empty" ⇒
        require(args.size == 1)
        Ins(m ⇒ {
          val l = args.head
          l.run(m)
          m.visitTypeInsn(INSTANCEOF, "lime/Nil")
          m.visitInsn(I2L)
        }, "J")
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
      case "int" ⇒
        require(args.size == 1)
        Ins(m ⇒ {
          val s = args.head
          s.run(m)
          m.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;", false)
          m.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(Ljava/lang/String;)Ljava/lang/Long;", false)
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
