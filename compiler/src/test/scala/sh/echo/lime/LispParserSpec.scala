package sh.echo.lime

import org.scalatest._

class LispParserSpec extends FunSpec with ShouldMatchers {

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

  describe("bools") {
    it("converts true to 1") {
      p.parse("""(def fun ()
                |  true)""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(),
          Atom(1)
        )
    }

    it("converts false to 0") {
      p.parse("""(def fun ()
                |  false)""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(),
          Atom(0)
        )
    }
  }

  describe("list sugar") {
    it("desugars quotes into a list function call") {
      p.parse("""(def fun ()
                |  '(1 2 3))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(),
          Exprs(
            Atom("list"),
            Atom(1),
            Atom(2),
            Atom(3)
          )
        )
    }

    it("desugars nested quotes into list function calls") {
      p.parse("""(def fun ()
                |  '(1 2 (3 (4 5) 6)))""".stripMargin) shouldBe
        Exprs(
          Atom("def"),
          Atom("fun"),
          Exprs(),
          Exprs(
            Atom("list"),
            Atom(1),
            Atom(2),
            Exprs(
              Atom("list"),
              Atom(3),
              Exprs(
                Atom("list"),
                Atom(4),
                Atom(5)
              ),
              Atom(6)
            )
          )
        )
    }
  }
}
