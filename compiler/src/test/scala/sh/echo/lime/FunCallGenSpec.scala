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

    it("calls a function by name") {
      val tc = compileAndLoad {
        """(def inc (a) (+ a 1))
          |(def foo (a b) (b a))
          |(def bar () (foo 1 inc))""".stripMargin
      }
      tc.bar() should be (2: JLong)
    }

    it("works for call-by-name functions with more than one arg") {
      val tc = compileAndLoad {
        """(def add (a b) (+ a b))
          |(def foo (a b c) (b a c))
          |(def bar (a) (foo a add 42))""".stripMargin
      }
      tc.bar(23: JLong) should be (65: JLong)
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
        s"INVOKESTATIC $testUnit.one ()$O",
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
        s"INVOKESTATIC $testUnit.foo ($O)$O",
        "ARETURN"
      )
    }

    it("can call another function regardless of declaration order") {
      val ms = compile {
        """(def foo () (one))
          |(def one () 1)""".stripMargin
      }
      ms(s"foo()$O") shouldBe List(
        s"INVOKESTATIC $testUnit.one ()$O",
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

    it("can pass a function as an arg and call it") {
      val ms = compile {
        """(def inc (a) (+ a 1))
          |(def foo (a b) (b a))
          |(def bar () (foo 1 inc))""".stripMargin
      }
      ms(s"inc($O)$O") shouldBe List(
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "LDC 1",
        "LADD",
        box("J"),
        "ARETURN"
      )
      ms(s"foo($O$O)$O") shouldBe List(
        "ALOAD 1",
        "CHECKCAST java/lang/reflect/Method",
        "ACONST_NULL", // object instance
        "LDC 1",       // begin varargs
        "ANEWARRAY java/lang/Object",
        "DUP",
        "LDC 0",
        "ALOAD 0",
        "AASTORE",     // end varargs
        s"INVOKEVIRTUAL java/lang/reflect/Method.invoke ($O[$O)$O",
        "ARETURN"
      )
      ms(s"bar()$O") shouldBe List(
        "LDC 1",
        box("J"),
        s"LDC L$testUnit;.class",
        "LDC \"inc\"",
        "LDC 1",       // begin varargs
        "ANEWARRAY java/lang/Class",
        "DUP",
        "LDC 0",
        s"LDC $O.class",
        "AASTORE",     // end varargs
        "INVOKEVIRTUAL java/lang/Class.getMethod (Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;",
        s"INVOKESTATIC $testUnit.foo ($O$O)$O",
        "ARETURN"
      )
    }
  }
}
