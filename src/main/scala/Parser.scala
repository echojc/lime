import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers {

  def parse(code: String): ParseResult[lime.List] = super.parse(exprs, code)

  def exprs: Parser[lime.List] = rep1(expr) ^^ listify
  def expr: Parser[Object] = number | string | symbol | list | quotes

  def number: Parser[Object] = """-?(\d+(\.\d*)?|\d*\.\d+)\b""".r ^^ (java.lang.Double.valueOf)
  def string: Parser[Object] = stringLiteral ^^ (s ⇒ s.substring(1, s.length - 1))
  def symbol: Parser[Object] = """[^\d\(\)'`,\s][^\(\)'`,\s]*""".r ^^ Symbol.apply
  def list: Parser[lime.List] = "(" ~> rep(expr) <~ ")" ^^ listify

  def quotes: Parser[Object] = quote | quasiquote | unquote

  def quote: Parser[lime.List] = "'" ~> expr ^^ (e ⇒ listify(List('quote, e)))
  def quasiquote: Parser[lime.List] = "`" ~> expr ^^ (e ⇒ listify(List('quasiquote, e)))
  def unquote: Parser[lime.List] = "," ~> expr ^^ (e ⇒ listify(List('unquote, e)))

  def listify(l: List[Object]): lime.List =
    l.foldRight(lime.Nil.get) { (nxt, acc) ⇒
      val converted =
        nxt match {
          case l: List[_] ⇒ listify(l.asInstanceOf[List[Object]])
          case v          ⇒ v
        }
      new lime.Cons(converted, acc)
    }
}
