import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {

  def parse(code: String): ParseResult[Any] = super.parse(expr, code)

  def expr = number | string | symbol | list | quotes

  def number: Parser[Any] = """-?(\d+(\.\d*)?|\d*\.\d+)\b""".r ^^ (_.toDouble)
  def string: Parser[Any] = stringLiteral ^^ (s ⇒ s.substring(1, s.length - 1))
  def symbol: Parser[Any] = """[^\d\(\)'`,\s][^\(\)'`,\s]*""".r ^^ Symbol.apply
  def list: Parser[Any] = "(" ~> rep(expr) <~ ")"

  def quotes: Parser[Any] = quote | quasiquote | unquote

  def quote: Parser[Any] = "'" ~> expr ^^ (e ⇒ List('quote, e))
  def quasiquote: Parser[Any] = "`" ~> expr ^^ (e ⇒ List('quasiquote, e))
  def unquote: Parser[Any] = "," ~> expr ^^ (e ⇒ List('unquote, e))
}
