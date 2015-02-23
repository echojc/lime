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
    it("cons") {
      val t = compile { """
        (def (foo) (cons 1 (cons 2 '())))
      """ }
      t.foo() shouldBe limeEquivalentOf(List(1: D, 2: D))
    }
    it("cons dotted pairs") {
      val t = compile { """
        (def (foo) (cons 1 (cons 2 3)))
      """ }
      val list = t.foo().asInstanceOf[lime.List]
      list.car() shouldBe 1
      list.cdr().asInstanceOf[lime.List].car() shouldBe 2
      list.cdr().asInstanceOf[lime.List].cdr() shouldBe 3
    }
    it("length, typical list") {
      val t = compile { """
        (def (foo) (length '(1 2.5 "3")))
      """ }
      t.foo() shouldBe 3
    }
    it("length, empty list") {
      val t = compile { """
        (def (foo) (length '()))
      """ }
      t.foo() shouldBe 0
    }
    it("length, not a list") {
      val t = compile { """
        (def (foo) (length (cons 1 2)))
      """ }
      an[Exception] should be thrownBy t.foo()
    }
  }
}
