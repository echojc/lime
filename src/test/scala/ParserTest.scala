import org.scalatest._

class ParserTest extends FunSpec with ShouldMatchers {
  import Parser.parse

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
  }

  describe("strings") {
    it("parses a string") {
      parse("\"hello\"") shouldBe "hello"
    }
    it("parses an empty string") {
      parse("\"\"") shouldBe ""
    }
  }

  describe("symbols") {
    it("parses a symbol") {
      parse("world") shouldEqual 'world
    }
  }
}
