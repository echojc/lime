package sh.echo.lime

class FunCallGenSpec extends GenBaseSpec {

  describe("integration tests") {
    it("calls another function") {
      val tc = compileAndLoad {
        """(def one () 1)
          |(def foo () (one))""".stripMargin
      }
      tc.foo() should be (1: JLong)
    }

    it("calls another function with parameters") {
      val tc = compileAndLoad {
        """(def inc (a) (+ a 1))
          |(def foo (a) (inc a))""".stripMargin
      }
      tc.foo(23: JLong) should be (24: JLong)
    }
  }

  describe("codegen") {
    it("can call another function") {
      val ms = compile {
        """(def one () 1)
          |(def foo () (one))""".stripMargin
      }
      ms(s"one()$O") shouldBe List(
        "LDC 1",
        box("J"),
        "ARETURN"
      )
      ms(s"foo()$O") shouldBe List(
        s"INVOKESTATIC $testUnit.one ()Ljava/lang/Object;",
        "ARETURN"
      )
    }

    it("can call a function with arguments") {
      val ms = compile {
        """(def foo (a) a)
          |(def bar () (foo 1))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        "ARETURN"
      )
      ms(s"bar()$O") shouldBe List(
        "LDC 1",
        box("J"),
        s"INVOKESTATIC $testUnit.foo (Ljava/lang/Object;)Ljava/lang/Object;",
        "ARETURN"
      )
    }

    it("can call another function regardless of declaration order") {
      val ms = compile {
        """(def foo () (one))
          |(def one () 1)""".stripMargin
      }
      ms(s"foo()$O") shouldBe List(
        s"INVOKESTATIC $testUnit.one ()Ljava/lang/Object;",
        "ARETURN"
      )
      ms(s"one()$O") shouldBe List(
        "LDC 1",
        box("J"),
        "ARETURN"
      )
    }

    it("throws an exception when a function cannot be found") {
      val ex = intercept[UnknownFunctionException] {
        compile {
          """(def foo ()
            |  (doesntexist))""".stripMargin
        }
      }
      ex.missingFun shouldBe "doesntexist"
    }
  }
}
