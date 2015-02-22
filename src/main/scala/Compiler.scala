import org.objectweb.asm._, Ops._

object Compiler {
  val O = "Ljava/lang/Object;"

  def genDesc(args: List[_]): String =
    s"(${Seq.fill(args.size)(O).mkString})$O"

  def compile(asts: List[Object], unit: String): Array[Byte] = {
    val c = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    c.visit(V1_7, ACC_PUBLIC + ACC_SUPER, unit, null, "java/lang/Object", null)

    asts foreach compileAst(c)

    c.visitEnd()
    c.toByteArray()
  }

  def compileAst(c: ClassWriter)(ast: Object): Unit = ast match {
    case 'def :: ((name: Symbol) :: args) :: bodys ⇒
      val m = c.visitMethod(ACC_PUBLIC + ACC_STATIC, name.name, genDesc(args), null, Array[String]("java/lang/Exception"))
      m.visitCode()
      bodys foreach (b ⇒ compileAst(m)(b.asInstanceOf[Object]))
      m.visitMaxs(0, 0)
      m.visitEnd()
  }

  def compileAst(m: MethodVisitor)(ast: Object): Unit = ast match {
    case d: java.lang.Double ⇒
      m.visitLdcInsn(d)
      m.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false);
      m.visitInsn(ARETURN)
  }
}
