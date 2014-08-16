package sh.echo.lime

import org.scalatest._

class LispParserSpec extends FunSpec with ShouldMatchers {

  val p = new LispParser

  it("parses basic lisp") {
    p.parse("""(def fun (x)
              |  (+ x 2))""".stripMargin) shouldBe
    List(
      Exprs(
        Ident("def"),
        Ident("fun"),
        Exprs(
          Ident("x")
        ),
        Exprs(
          Ident("+"),
          Ident("x"),
          NumberConst(2)
        )
      )
    )
  }

  it("accepts empty lists") {
    p.parse("""(def fun (x)
              |  ())""".stripMargin) shouldBe
    List(
      Exprs(
        Ident("def"),
        Ident("fun"),
        Exprs(
          Ident("x")
        ),
        Exprs()
      )
    )
  }

  it("parses many expressions") {
    p.parse("""(def a () 1)
              |(def b ()
              |  (+ 2 3))""".stripMargin) shouldBe
    List(
      Exprs(
        Ident("def"),
        Ident("a"),
        Exprs(),
        NumberConst(1)
      ),
      Exprs(
        Ident("def"),
        Ident("b"),
        Exprs(),
        Exprs(
          Ident("+"),
          NumberConst(2),
          NumberConst(3)
        )
      )
    )
  }

  it("parses naked atoms") {
    p.parse("""2
              |true
              |(def a () 3)""".stripMargin) shouldBe
    List(
      NumberConst(2),
      NumberConst(1),
      Exprs(
        Ident("def"),
        Ident("a"),
        Exprs(),
        NumberConst(3)
      )
    )
  }

  describe("parsing whole numbers") {
    it("parses numbers that start with 0") {
      p.parse("""(def fun (x)
                |  (+ x 012))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(
            Ident("x")
          ),
          Exprs(
            Ident("+"),
            Ident("x"),
            NumberConst(12)
          )
        )
      )
    }

    it("works on negative numbers") {
      p.parse("""(def fun (x)
                |  (+ x -2))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(
            Ident("x")
          ),
          Exprs(
            Ident("+"),
            Ident("x"),
            NumberConst(-2)
          )
        )
      )
    }
  }

  describe("parsing decimal numbers") {
    it("parses simple decimals") {
      p.parse("""(def fun (x)
                |  (+ x 2.0))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(
            Ident("x")
          ),
          Exprs(
            Ident("+"),
            Ident("x"),
            DecimalConst(2.0)
          )
        )
      )
    }

    it("parses negative decimals") {
      p.parse("""(def fun (x)
                |  (+ x -2.0))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(
            Ident("x")
          ),
          Exprs(
            Ident("+"),
            Ident("x"),
            DecimalConst(-2.0)
          )
        )
      )
    }

    it("parses decimals with a dot but no numbers after") {
      p.parse("""(def fun (x)
                |  (+ x 2.))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(
            Ident("x")
          ),
          Exprs(
            Ident("+"),
            Ident("x"),
            DecimalConst(2.0)
          )
        )
      )
    }

    it("rejects atoms that start with a dot with numbers after as a decimal") {
      p.parse("""(def fun (x)
                |  (+ x .2))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(
            Ident("x")
          ),
          Exprs(
            Ident("+"),
            Ident("x"),
            Ident(".2")
          )
        )
      )
    }
  }

  describe("bools") {
    it("converts true to 1") {
      p.parse("""(def fun ()
                |  true)""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(),
          NumberConst(1)
        )
      )
    }

    it("converts false to 0") {
      p.parse("""(def fun ()
                |  false)""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(),
          NumberConst(0)
        )
      )
    }
  }

  describe("list sugar") {
    it("desugars 'nil' into an empty list") {
      p.parse("(def fun () nil)") shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(),
          Exprs(
            Ident("list")
          )
        )
      )
    }

    it("desugars quotes into a list function call") {
      p.parse("""(def fun ()
                |  '(1 2 3))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(),
          Exprs(
            Ident("list"),
            NumberConst(1),
            NumberConst(2),
            NumberConst(3)
          )
        )
      )
    }

    it("desugars nested quotes into list function calls") {
      p.parse("""(def fun ()
                |  '(1 2 (3 (4 5) 6)))""".stripMargin) shouldBe
      List(
        Exprs(
          Ident("def"),
          Ident("fun"),
          Exprs(),
          Exprs(
            Ident("list"),
            NumberConst(1),
            NumberConst(2),
            Exprs(
              Ident("list"),
              NumberConst(3),
              Exprs(
                Ident("list"),
                NumberConst(4),
                NumberConst(5)
              ),
              NumberConst(6)
            )
          )
        )
      )
    }
  }
}
