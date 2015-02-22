import java.lang.{ Double ⇒ D }
class InlineTest extends LimeSpec {
  describe("math ops") {
    it("addition") {
      val t = compile { """
        (def (foo) (+ 1 2))
      """ }
      t.foo() shouldBe 3
    }
    it("minus") {
      val t = compile { """
        (def (foo) (- 1 2))
      """ }
      t.foo() shouldBe -1
    }
    it("times") {
      val t = compile { """
        (def (foo) (* 1 2))
      """ }
      t.foo() shouldBe 2
    }
    it("divide") {
      val t = compile { """
        (def (foo) (/ 1 2))
      """ }
      t.foo() shouldBe 0.5
    }
    it("mod") {
      val t = compile { """
        (def (foo) (% 5.5 3))
      """ }
      t.foo() shouldBe 2.5
    }
  }

  describe("list ops") {
    it("car") {
      val t = compile { """
        (def (foo) (car '(1 2)))
      """ }
      t.foo() shouldBe 1
    }
    it("cdr") {
      val t = compile { """
        (def (foo) (cdr '(1 2)))
      """ }
      t.foo() shouldBe limeEquivalentOf(List(2: D))
    }
  }
}
