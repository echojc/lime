package sh.echo.exw

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex.Match

trait Expr
case class Atom(value: Any) extends Expr
case class Exprs(exprs: List[Expr]) extends Expr

object Exprs {
  def apply(exprs: Expr*): Exprs =
    Exprs(exprs.toList)
}

object Atom {
  val WholeNumberRegex = """^(-?[0-9]+)$""".r
  def typedApply(value: Any): Atom =
    WholeNumberRegex.findFirstIn(value.toString) match {
      case Some(num) ⇒ Atom(num.toLong)
      case _         ⇒ Atom(value.toString)
    }
}


class LispParser extends JavaTokenParsers {
  case class ParseException(error: NoSuccess) extends RuntimeException
  val AtomRegex = """[^\s()]+""".r

  def parse(input: String): Expr =
    parseAll(list, input) match {
      case Success(result, _) ⇒ result
      case error: NoSuccess   ⇒ throw new ParseException(error)
    }

  def expr: Parser[Expr] = list | atom
  def list: Parser[Expr] = ("(" ~> rep(expr) <~ ")") ^^ Exprs.apply
  def atom: Parser[Expr] = regex(AtomRegex) ^^ Atom.typedApply
}
