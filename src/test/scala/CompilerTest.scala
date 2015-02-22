import scala.language.dynamics
import org.scalatest._

trait CompilerTest extends FunSpec with ShouldMatchers {
  def compile(code: String): CallableClass = {
    val ast = Parser.parse(code).get
    val bc = Compiler.compile(ast, "$test")
    new CallableClass(bc)
  }
}

// sugar up those function calls
class CallableClass(bytecode: Array[Byte]) extends ClassLoader with Dynamic {
  val underlyingClass: Class[_] = defineClass("$test", bytecode, 0, bytecode.size)
  def applyDynamic(name: String)(args: Object*): Object = {
    val m = underlyingClass.getMethod(name, Seq.fill(args.size)(classOf[Object]): _*)
    m.invoke(null, args: _*)
  }
  def selectDynamic(name: String): Object = {
    val f = underlyingClass.getField(name)
    f.get(null)
  }
}
