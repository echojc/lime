import scala.language.dynamics
import org.scalatest._

class CompilerTest extends FunSpec with ShouldMatchers {

  it("binds values to names") {
    val t = compile { """
      (def (foo) 1)
    """ }
    t.foo() shouldBe 1.0
  }

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
}
