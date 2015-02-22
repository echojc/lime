import scala.language.dynamics
import org.scalatest._, matchers.{ ShouldMatchers ⇒ _, _ }

trait LimeSpec extends FunSpec with ShouldMatchers {
  def limeEquivalentOf(right: List[Object]) = BeMatcher { left: Object ⇒
    def convertList(list: lime.List): List[Object] =
      Stream.iterate((List.empty[Object], list)) {
        case (acc, list) ⇒
          val car = list.car() match {
            case list: lime.List ⇒ convertList(list)
            case v               ⇒ v
          }
          (acc :+ car, list.cdr().asInstanceOf[lime.List])
      }.dropWhile(!_._2.isInstanceOf[lime.Nil]).head._1

    left match {
      case left: lime.List ⇒
        val converted = convertList(left)
        MatchResult(
          converted == right,
          s"$converted was not equal to $right",
          s"$converted was equal to $right"
        )
      case _ ⇒
        MatchResult(false, s"$left was not a lime.List", s"$left was a lime.List")
    }
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
  def selectDynamic(name: String): Object = {
    val f = underlyingClass.getField(name)
    f.get(null)
  }
}
