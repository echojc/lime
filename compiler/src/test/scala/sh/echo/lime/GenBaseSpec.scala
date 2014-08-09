package sh.echo.lime

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.language.dynamics

import org.objectweb.asm._
import org.objectweb.asm.util._
import org.scalatest._

object GenBaseSpec {
  import ClassGen._

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
}

class TestClassLoader extends ClassLoader {
  def load(name: String, bytes: Array[Byte]): Class[_] =
    defineClass(name, bytes, 0, bytes.length)
}

// sugar up those function calls
class CallableClass(underlyingClass: Class[_]) extends Dynamic {
  def applyDynamic(fun: String)(args: Object*): Object = {
    val m = underlyingClass.getMethod(fun, Seq.fill(args.size)(classOf[Object]): _*)
    m.invoke(null, args: _*)
  }
}

trait GenBaseSpec extends FunSpec with ShouldMatchers {
  import ClassGen.paramsFor
  import GenBaseSpec.InsnExtractor

  val O = ClassGen.O
  val testUnit = "$test"

  def checkCast(tpe: String) = {
    val (ot, _) = paramsFor(tpe)
    s"CHECKCAST $ot"
  }

  def unbox(tpe: String) = {
    val (ot, em) = paramsFor(tpe)
    s"INVOKEVIRTUAL $ot.$em ()$tpe"
  }

  def box(tpe: String) = {
    val (ot, _) = paramsFor(tpe)
    s"INVOKESTATIC $ot.valueOf ($tpe)L$ot;"
  }

  def compile(lisp: String): Map[String, List[String]] = {
    val lp = new LispParser
    val cg = new ClassGen
    val bc = cg.compileUnit(testUnit, lp.parse(lisp))
    InsnExtractor.parse(bc)
  }

  def compileAndLoad(lisp: String): CallableClass = {
    val lp = new LispParser
    val cg = new ClassGen
    val bc = cg.compileUnit(testUnit, lp.parse(lisp))
    val cl = (new TestClassLoader).load(testUnit, bc)
    new CallableClass(cl)
  }
}
