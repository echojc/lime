class InlineTest extends CompilerTest {
  it("addition") {
    val t = compile { """
      (def (foo) (+ 1 2))
    """ }
    t.foo() shouldBe 3.0
  }
  it("minus") {
    val t = compile { """
      (def (foo) (- 1 2))
    """ }
    t.foo() shouldBe -1.0
  }
  it("times") {
    val t = compile { """
      (def (foo) (* 1 2))
    """ }
    t.foo() shouldBe 2.0
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
