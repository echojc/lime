package sh.echo.lime

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex.Match

trait Expr
trait Atom extends Expr
case class Ident(name: String) extends Atom
trait Const extends Atom
case class DecimalConst(value: Double) extends Const
case class NumberConst(value: Long) extends Const
case class Exprs(exprs: List[Expr]) extends Expr

object Exprs {
  def apply(exprs: Expr*): Exprs =
    Exprs(exprs.toList)

  def applyList(exprs: List[Expr]): Exprs = {
    def iter(exprs: List[Expr]): List[Expr] =
      exprs map (_ match {
        case Exprs(exprs) ⇒ Exprs(Ident("list") :: iter(exprs))
        case rest         ⇒ rest
      })
    Exprs(Ident("list") :: iter(exprs))
  }
}

class LispParser extends JavaTokenParsers {
  class ParseException(error: NoSuccess) extends Exception(error.toString)
  val AtomRegex = """[^\s()]+""".r
  val DecimalRegex = """-?[0-9]+\.[0-9]*""".r
  val NumberRegex = """-?[0-9]+""".r

  def parse(input: String): List[Expr] =
    parseAll(file, input) match {
      case Success(result, _) ⇒ result
      case error: NoSuccess   ⇒ throw new ParseException(error)
    }

  def file: Parser[List[Expr]] = rep(expr)

  def exprs: Parser[Expr] = ("(" ~> rep(expr) <~ ")") ^^ Exprs.apply
  def expr: Parser[Expr] = exprs | list | keywords | atom
  def atom: Parser[Expr] = decimal | whole | identifier

  def whole: Parser[Expr] = regex(NumberRegex) ^^ (n ⇒ NumberConst(n.toLong))
  def decimal: Parser[Expr] = regex(DecimalRegex) ^^ (d ⇒ DecimalConst(d.toDouble))
  def identifier: Parser[Expr] = regex(AtomRegex) ^^ (s ⇒ Ident(s.toString))

  def keywords: Parser[Expr] = ktrue | kfalse | knil
  def ktrue: Parser[Expr] = "true" ^^ { _ ⇒ NumberConst(1) }
  def kfalse: Parser[Expr] = "false" ^^ { _ ⇒ NumberConst(0) }
  def knil: Parser[Expr] = "nil" ^^ { _ ⇒ Exprs.applyList(Nil) }

  // sugar
  def list: Parser[Expr] = ("'(" ~> rep(expr) <~ ")") ^^ { Exprs.applyList }
}
