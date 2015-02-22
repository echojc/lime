import org.scalatest._

class ParserTest extends FunSpec with ShouldMatchers {

  def parseAll(code: String): Object = Parser.parse(code).get
  def parse(code: String): Object = parseAll(code).asInstanceOf[List[Object]].head

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
      parse("()") shouldBe Nil
    }
    it("parses a list with one expr") {
      parse("(foo)") shouldBe List('foo)
    }
    it("parses a list with more than one expr") {
      parse("(foo 1 2.5 \"3\")") shouldBe List('foo, 1.0, 2.5, "3")
    }
    it("parses nested lists") {
      parse("(foo () (1 2.5 \"3\") bar)") shouldBe List('foo, Nil, List(1.0, 2.5, "3"), 'bar)
    }
  }

  describe("quotes") {
    Seq(
      "'" → "quote",
      "`" → "quasiquote",
      "," → "unquote"
    ) foreach { case (char, name) ⇒
      it(s"parses $char on a single expr into $name") {
        parse(s"${char}foo") shouldBe List(Symbol(name), 'foo)
      }
      it(s"parses $char on a list into $name") {
        parse(s"$char(foo bar)") shouldBe List(Symbol(name), List('foo, 'bar))
      }
      it(s"parses nested $char") {
        parse(s"${char}${char}foo") shouldBe List(Symbol(name), List(Symbol(name), 'foo))
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
      """ } shouldBe
      List('def, List('foldl, 'fun, 'zero, 'xs),
        List('if, List(Symbol("nil?"), 'xs),
          'zero,
          List('fun, List('foldl, 'fun, 'zero, List('cdr, 'xs)), List('car, 'xs))))
    }
    it("parses multiple expressions") {
      parseAll { """
        (def (foo) 1)
        (def (bar) 2)
      """ } shouldBe
      List(
        List('def, List('foo), 1.0),
        List('def, List('bar), 2.0)
      )
    }
  }
}
