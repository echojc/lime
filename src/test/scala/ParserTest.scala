import java.lang.{ Double ⇒ D }
import org.scalatest._, matchers.{ ShouldMatchers ⇒ _, _ }

class ParserTest extends FunSpec with ShouldMatchers {

  def parseAll(code: String): lime.List = Parser.parse(code).get
  def parse(code: String): Object = parseAll(code).asInstanceOf[lime.List].car()

  describe("numbers") {
    it("parses a decimal as double") {
      parse("1.5") shouldBe 1.5
    }
    it("parses an integer as double") {
      parse("1") shouldBe 1.0
    }
    it("parses a negative decimal as double") {
      parse("-1.5") shouldBe -1.5
    }
    it("parses a negative integer as double") {
      parse("-1") shouldBe -1.0
    }
    it("errors if a number isn't followed by a word boundary") {
      Parser.parse("3a") shouldBe a[Parser.NoSuccess]
    }
  }

  describe("strings") {
    it("parses a string") {
      parse("\"hello\"") shouldBe "hello"
    }
    it("parses an empty string") {
      parse("\"\"") shouldBe ""
    }
    it("parses escaped characters")(pending)
  }

  describe("symbols") {
    it("parses a symbol") {
      parse("world") shouldEqual 'world
    }
    it("parses special characters as symbols") {
      parse("+") shouldEqual Symbol("+")
    }
    it("parses special characters as part of symbols") {
      parse("nil?") shouldEqual Symbol("nil?")
    }
  }

  describe("lists") {
    it("parses an empty list") {
      parse("()") shouldBe limeEquivalentOf(Nil)
    }
    it("parses a list with one expr") {
      parse("(foo)") shouldBe limeEquivalentOf(List('foo))
    }
    it("parses a list with more than one expr") {
      parse("(foo 1 2.5 \"3\")") shouldBe limeEquivalentOf(List('foo, 1.0: D, 2.5: D, "3"))
    }
    it("parses nested lists") {
      parse("(foo () (1 2.5 \"3\") bar)") shouldBe limeEquivalentOf(List('foo, Nil, List(1.0, 2.5, "3"), 'bar))
    }
  }

  describe("quotes") {
    Seq(
      "'" → "quote",
      "`" → "quasiquote",
      "," → "unquote"
    ) foreach { case (char, name) ⇒
      it(s"parses $char on a single expr into $name") {
        parse(s"${char}foo") shouldBe limeEquivalentOf(List(Symbol(name), 'foo))
      }
      it(s"parses $char on a list into $name") {
        parse(s"$char(foo bar)") shouldBe limeEquivalentOf(List(Symbol(name), List('foo, 'bar)))
      }
      it(s"parses nested $char") {
        parse(s"${char}${char}foo") shouldBe limeEquivalentOf(List(Symbol(name), List(Symbol(name), 'foo)))
      }
    }
  }

  describe("integration") {
    it("parses a typical expression") {
      parse { """
        (def (foldl fun zero xs)
          (if (nil? xs)
            zero
            (fun (foldl fun zero (cdr xs)) (car xs))))
      """ } shouldBe limeEquivalentOf(
        List('def, List('foldl, 'fun, 'zero, 'xs),
          List('if, List(Symbol("nil?"), 'xs),
            'zero,
            List('fun, List('foldl, 'fun, 'zero, List('cdr, 'xs)), List('car, 'xs))))
      )
    }
    it("parses multiple expressions") {
      parseAll { """
        (def (foo) 1)
        (def (bar) 2)
      """ } shouldBe limeEquivalentOf(
        List(
          List('def, List('foo), 1.0),
          List('def, List('bar), 2.0)
        )
      )
    }
  }

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
}
