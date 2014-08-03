package sh.echo.exw

import org.scalatest._

class LispParserTest extends FunSpec with ShouldMatchers {

  val p = new LispParser

  it("parses basic lisp") {
    p.parse("""(def fun (x)
              |  (+ x 2))""".stripMargin) shouldBe
      Exprs(
        Atom("def"),
        Atom("fun"),
        Exprs(
          Atom("x")
        ),
        Exprs(
          Atom("+"),
          Atom("x"),
          Atom(2)
        )
      )
  }

  it("accepts empty lists") {
    p.parse("""(def fun (x)
              |  ())""".stripMargin) shouldBe
      Exprs(
        Atom("def"),
        Atom("fun"),
        Exprs(
          Atom("x")
        ),
        Exprs()
      )
  }

  it("only splits atoms on whitespace") {
    p.parse("""(def 0fun-hyphen (x)
              |  (+ x1 2))""".stripMargin) shouldBe
      Exprs(
        Atom("def"),
        Atom("0fun-hyphen"),
        Exprs(
          Atom("x")
        ),
        Exprs(
          Atom("+"),
          Atom("x1"),
          Atom(2)
        )
      )
  }

  describe("parsing whole numbers") {
    it("parses numbers that start with 0") {
      p.parse("""(def fun (x)
                |  (+ x 012))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(
            Atom("x")
          ),
          Exprs(
            Atom("+"),
            Atom("x"),
            Atom(12)
          )
        )
    }

    it("works on negative numbers") {
      p.parse("""(def fun (x)
                |  (+ x -2))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(
            Atom("x")
          ),
          Exprs(
            Atom("+"),
            Atom("x"),
            Atom(-2)
          )
        )
    }
  }

  describe("parsing decimal numbers") {
    it("parses simple decimals") {
      p.parse("""(def fun (x)
                |  (+ x 2.0))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(
            Atom("x")
          ),
          Exprs(
            Atom("+"),
            Atom("x"),
            Atom(2.0)
          )
        )
    }

    it("parses negative decimals") {
      p.parse("""(def fun (x)
                |  (+ x -2.0))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(
            Atom("x")
          ),
          Exprs(
            Atom("+"),
            Atom("x"),
            Atom(-2.0)
          )
        )
    }

    it("parses decimals with a dot but no numbers after") {
      p.parse("""(def fun (x)
                |  (+ x 2.))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(
            Atom("x")
          ),
          Exprs(
            Atom("+"),
            Atom("x"),
            Atom(2.0)
          )
        )
    }

    it("does not parse atoms that start with a dot with numbers after as a decimal") {
      p.parse("""(def fun (x)
                |  (+ x .2))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(
            Atom("x")
          ),
          Exprs(
            Atom("+"),
            Atom("x"),
            Atom(".2")
          )
        )
    }
  }
}
