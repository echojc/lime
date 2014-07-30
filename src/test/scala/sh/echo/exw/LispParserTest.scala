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
