import java.lang.{ Double ⇒ D }

class BasicTest extends LimeSpec {
  describe("atoms") {
    it("compiles") {
      val t = compile { """
        (def (foo) 3.14159)
        (def (bar) "abc")
        (def (baz) 1)
      """ }
      t.foo() shouldBe 3.14159
      t.bar() shouldBe "abc"
      t.baz() shouldBe 1
    }
    it("compiles when quoted") {
      val t = compile { """
        (def (qfoo) '3.14159)
        (def (qbar) '"abc")
        (def (qbaz) '1)
      """ }
      t.qfoo() shouldBe 3.14159
      t.qbar() shouldBe "abc"
      t.qbaz() shouldBe 1
    }
    it("symbols") {
      val t = compile { """
        (def (foo) 'bar)
      """ }
      t.foo() shouldEqual 'bar
    }
    it("nested symbols") {
      val t = compile { """
        (def (foo) '''bar)
      """ }
      t.foo() shouldBe limeEquivalentOf(List('quote, List('quote, 'bar)))
    }
  }

  describe("lists") {
    it("empty list") {
      val t = compile { """
        (def (foo) '())
      """ }
      t.foo() shouldBe limeEquivalentOf(Nil)
    }
    it("non-empty list") {
      val t = compile { """
        (def (foo) '(1 2.5 "3" 'bar))
      """ }
      t.foo() shouldBe limeEquivalentOf(List(1: D, 2.5: D, "3", List('quote, 'bar)))
    }
    it("nested lists") {
      val t = compile { """
        (def (foo) '(() (1 2.5 "3") 'bar))
      """ }
      t.foo() shouldBe limeEquivalentOf(List(Nil, List(1: D, 2.5: D, "3"), List('quote, 'bar)))
    }
    it("nested quoted lists") {
      val t = compile { """
        (def (foo) '('() ''('(1 2.5 "3") 'bar)))
      """ }
      t.foo() shouldBe limeEquivalentOf(List(
        List('quote, Nil), List('quote, List('quote, List(List('quote, List(1: D, 2.5: D, "3")), List('quote, 'bar))))
      ))
    }
  }
}
