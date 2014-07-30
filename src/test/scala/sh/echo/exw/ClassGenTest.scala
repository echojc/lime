package sh.echo.exw

import scala.collection.JavaConversions._
import scala.collection.mutable

import org.objectweb.asm._
import org.objectweb.asm.util._
import org.scalatest._

class ClassGenTest extends FunSpec with ShouldMatchers {

  it("compiles a simple addition function") {
    val ms = compile {
      """(defn foo (a)
        |  (+ a 23))""".stripMargin
    }
    ms("foo(J)J") shouldBe List(
      "LLOAD 0",
      "LDC 23",
      "LADD",
      "LRETURN"
    )
  }

  it("compiles nested addition functions") {
    val ms = compile {
      """(defn foo (a)
        |  (+ a (+ (+ 23 a) 42)))""".stripMargin
    }
    ms("foo(J)J") shouldBe List(
      "LLOAD 0",
      "LDC 23",
      "LLOAD 0",
      "LADD",
      "LDC 42",
      "LADD",
      "LADD",
      "LRETURN"
    )
  }

  def compile(lisp: String): Map[String, List[String]] = {
    val lp = new LispParser
    val cg = new ClassGen
    val bc = cg.compile(lp.parse(lisp))
    InsnExtractor.parse(bc)
  }
}

object InsnExtractor {
  def parse(bc: Array[Byte]): Map[String, List[String]] = {
    val cr = new ClassReader(bc)
    val ex = new InsnExtractor()
    cr.accept(ex, 0)
    ex.methods
  }
}
class InsnExtractor extends ClassVisitor(Opcodes.ASM5) {
  private val data = mutable.Map.empty[String, Textifier]

  def methods: Map[String, List[String]] =
    data.mapValues { t ⇒
      val data = t.getText.map(_.toString.trim).toList
      data.filterNot(i ⇒ Set("MAXSTACK", "MAXLOCALS").exists(i.contains))
    }.toMap

  override def visitMethod(
    access: Int,
    name: String,
    desc: String,
    signature: String,
    exceptions: Array[String]): MethodVisitor = {

    val key = name + desc
    val t = data.getOrElseUpdate(key, new Textifier())
    new TraceMethodVisitor(t)
  }
}

  //it("") {
  //  val bc = (new ClassGen).go()
  //  val ms = readMethods(bc)

  //  ms("main([Ljava/lang/String;)V") shouldBe List(
  //    "GETSTATIC java/lang/System.out : Ljava/io/PrintStream;",
  //    "LDC \"woot\"",
  //    "INVOKEVIRTUAL java/io/PrintStream.println (Ljava/lang/String;)V",
  //    "RETURN"
  //  )
  //}
