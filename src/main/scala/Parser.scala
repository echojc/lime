import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {

  def parse(code: String): ParseResult[List[Object]] = super.parse(exprs, code)

  def exprs: Parser[List[Object]] = rep1(expr)
  def expr: Parser[Object] = number | string | symbol | list | quotes

  def number: Parser[Object] = """-?(\d+(\.\d*)?|\d*\.\d+)\b""".r ^^ (java.lang.Double.valueOf)
  def string: Parser[Object] = stringLiteral ^^ (s ⇒ s.substring(1, s.length - 1))
  def symbol: Parser[Object] = """[^\d\(\)'`,\s][^\(\)'`,\s]*""".r ^^ Symbol.apply
  def list: Parser[Object] = "(" ~> rep(expr) <~ ")"

  def quotes: Parser[Object] = quote | quasiquote | unquote

  def quote: Parser[Object] = "'" ~> expr ^^ (e ⇒ List('quote, e))
  def quasiquote: Parser[Object] = "`" ~> expr ^^ (e ⇒ List('quasiquote, e))
  def unquote: Parser[Object] = "," ~> expr ^^ (e ⇒ List('unquote, e))
}
