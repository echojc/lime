class BasicTest extends CompilerTest {
  it("binds single values to functions") {
    val t = compile { """
      (def (foo) 3.14159)
      (def (bar) "abc")
      (def (baz) 1)
    """ }
    t.foo() shouldBe 3.14159
    t.bar() shouldBe "abc"
    t.baz() shouldBe 1.0
  }
}
