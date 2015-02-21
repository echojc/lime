import scala.language.dynamics
import org.scalatest._

class CompilerTest extends FunSpec with ShouldMatchers {
  def compile(code: String): CallableClass = {
    val ast = Parser.parse(code).get
    val bc = Compiler.compile(ast)
    new CallableClass(bc)
  }
}

// sugar up those function calls
class CallableClass(bytecode: Array[Byte]) extends ClassLoader with Dynamic {
  val underlyingClass: Class[_] = defineClass("$test", bytecode, 0, bytecode.size)
  //def main(args: String*) = {
  //  val m = underlyingClass.getMethod("main", classOf[Array[String]])
  //  m.invoke(null, args.toArray)
  //}
  def applyDynamic(name: String)(args: Object*): Object = {
    val m = underlyingClass.getMethod(name, Seq.fill(args.size)(classOf[Object]): _*)
    m.invoke(null, args: _*)
  }
}
