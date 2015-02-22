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
        (def (foo) '(1 2.0 "3"))
      """ }
      t.foo() shouldBe limeEquivalentOf(List(1: D, 2.0: D, "3"))
    }
    it("nested lists") {
      val t = compile { """
        (def (foo) '(() (1 2.0 "3") 4))
      """ }
      t.foo() shouldBe limeEquivalentOf(List(Nil, List(1: D, 2.0: D, "3"), 4: D))
    }
  }
}
