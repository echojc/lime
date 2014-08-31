package sh.echo.lime

class LambdaGenSpec extends GenBaseSpec {

  describe("integration tests") {
    it("calls an inline lambda") {
      val tc = compileAndLoad {
        """(def foo ()
          |  ((fn (a) a) 1))""".stripMargin
      }
      tc.foo() should be (1: JLong)
    }

    it("calls a lambda passed by name") {
      val tc = compileAndLoad {
        """(def foo (a) (a))
          |(def bar () (foo (fn () 42)))""".stripMargin
      }
      tc.bar() should be (42: JLong)
    }
  }

  describe("codegen") {
    it("compiles a lambda") {
      val ms = compile {
        """(fn (a) a)"""
      }
      ms(s"$$fn$$0($O)$O") shouldBe List(
        "ALOAD 0",
        "ARETURN"
      )
    }

    it("calls a lambda") {
      val ms = compile {
        """(def foo ()
          |  ((fn (a) a) 1))""".stripMargin
      }
      ms(s"foo()$O") shouldBe List(
        "LDC 1",
        box("J"),
        s"INVOKESTATIC $testUnit.$$fn$$0 ($O)$O",
        "ARETURN"
      )
    }

    it("passes and calls a lambda by name") {
      val ms = compile {
        """(def foo (a) (a))
          |(def bar () (foo (fn () 42)))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        "CHECKCAST java/lang/reflect/Method",
        "ACONST_NULL",                // object instance
        "LDC 0",                      // begin varargs
        "ANEWARRAY java/lang/Object", // end varargs
        s"INVOKEVIRTUAL java/lang/reflect/Method.invoke ($O[$O)$O",
        "ARETURN"
      )
      ms(s"bar()$O") shouldBe List(
        s"LDC L$testUnit;.class",
        "LDC \"$fn$0\"",
        "LDC 0",                      // begin varargs
        "ANEWARRAY java/lang/Class",  // end varargs
        "INVOKEVIRTUAL java/lang/Class.getMethod (Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;",
        s"INVOKESTATIC $testUnit.foo ($O)$O",
        "ARETURN"
      )
    }
  }
}
