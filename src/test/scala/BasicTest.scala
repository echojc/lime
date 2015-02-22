class BasicTest extends CompilerTest {
  describe("atoms") {
    it("binds to functions") {
      val t = compile { """
        (def (foo) 3.14159)
        (def (bar) "abc")
        (def (baz) 1)
      """ }
      t.foo() shouldBe 3.14159
      t.bar() shouldBe "abc"
      t.baz() shouldBe 1
    }
    it("binds to functions when quoted") {
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
}
