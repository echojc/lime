package sh.echo.exw

import org.objectweb.asm._

class ClassGen {
  import Ops._

  def compile(expr: Expr): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    cw.visit(V1_7, ACC_PUBLIC + ACC_SUPER, "test", null, "java/lang/Object", null)

    expr match {
      case Exprs(Atom("defn") :: Atom(name) :: Exprs(args) :: body :: Nil) ⇒
        require(args forall (_.isInstanceOf[Atom]))
        val argNames = args.map(_.asInstanceOf[Atom].value.toString)
        compileMethod(cw, name.toString, argNames, body)
    }

    cw.visitEnd()
    cw.toByteArray()
  }

  def compileMethod(cw: ClassWriter, name: String, args: List[String], expr: Expr): Unit = {
    val m = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, name, "(JJ)J", null, null)
    m.visitCode()
    compileExpr(m, expr, args)
    m.visitInsn(LRETURN)
    m.visitMaxs(0, 0)
    m.visitEnd()
  }

  def compileExpr(m: MethodVisitor, expr: Expr, scopeArgs: List[String]): Unit = {
    expr match {
      case Atom(i: Int) ⇒
        m.visitLdcInsn(i)
      case Atom(s: String) ⇒
        require(scopeArgs.contains(s))
        m.visitVarInsn(LLOAD, scopeArgs.indexOf(s) * 2)
      case Exprs(Atom(fun: String) :: rest) ⇒
        rest.foreach(compileExpr(m, _, scopeArgs))
        compileFunCall(m, fun)
    }
  }

  def compileFunCall(m: MethodVisitor, fun: String): Unit = {
    fun match {
      case "+" ⇒
        m.visitInsn(LADD)
    }
  }
}

    //val ctor = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    //ctor.visitCode()
    //ctor.visitVarInsn(ALOAD, 0)
    //ctor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    //ctor.visitInsn(RETURN)
    //ctor.visitMaxs(0, 0) // automatic
    //ctor.visitEnd()

    //val main = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    //main.visitCode()
    //main.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    //main.visitLdcInsn("woot")
    //main.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
    //main.visitInsn(RETURN)
    //main.visitMaxs(0, 0) // ignored
    //main.visitEnd()
