package sh.echo.lime

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex.Match

trait Expr
case class Atom(value: Any) extends Expr
case class Exprs(exprs: List[Expr]) extends Expr

object Exprs {
  def apply(exprs: Expr*): Exprs =
    Exprs(exprs.toList)

  def applyList(exprs: List[Expr]): Exprs = {
    def iter(exprs: List[Expr]): List[Expr] =
      exprs map (_ match {
        case Exprs(exprs) ⇒ Exprs(Atom("list") :: iter(exprs))
        case rest         ⇒ rest
      })
    Exprs(Atom("list") :: iter(exprs))
  }
}

object Atom {
  val DecimalNumberRegex = """^(-?[0-9]+\.[0-9]*)$""".r
  val WholeNumberRegex = """^(-?[0-9]+)$""".r
  def typedApply(value: Any): Atom =
    WholeNumberRegex.findFirstIn(value.toString) match {
      case Some(num) ⇒
        Atom(num.toLong)
      case None ⇒
        DecimalNumberRegex.findFirstIn(value.toString) match {
          case Some(num) ⇒
            Atom(num.toDouble)
          case None ⇒
            Atom(value.toString)
        }
    }
}


class LispParser extends JavaTokenParsers {
  case class ParseException(error: NoSuccess) extends RuntimeException
  val AtomRegex = """[^\s()]+""".r

  def parse(input: String): Expr =
    parseAll(exprs, input) match {
      case Success(result, _) ⇒ result
      case error: NoSuccess   ⇒ throw new ParseException(error)
    }

  def exprs: Parser[Expr] = ("(" ~> rep(expr) <~ ")") ^^ Exprs.apply
  def expr: Parser[Expr] = exprs | list | bools | atom
  def atom: Parser[Expr] = regex(AtomRegex) ^^ Atom.typedApply

  def bools: Parser[Expr] = btrue | bfalse
  def btrue: Parser[Expr] = "true" ^^ { _ ⇒ Atom.typedApply(1) }
  def bfalse: Parser[Expr] = "false" ^^ { _ ⇒ Atom.typedApply(0) }

  // sugar
  def list: Parser[Expr] = ("'(" ~> rep(expr) <~ ")") ^^ { Exprs.applyList }
}
