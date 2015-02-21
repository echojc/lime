import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {

  def parse(code: String): Any = super.parse(expr, code).get

  def expr = number | string | symbol

  def number: Parser[Any] = """-?(\d+(\.\d*)?|\d*\.\d+)""".r ^^ (_.toDouble)
  def string: Parser[Any] = stringLiteral ^^ (s ⇒ s.substring(1, s.length - 1))
  def symbol: Parser[Any] = ident ^^ Symbol.apply
}
