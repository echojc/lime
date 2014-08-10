package sh.echo.lime

class IfGenSpec extends GenBaseSpec {

  describe("integration test") {
    it("runs the true branch when true") {
      val tc = compileAndLoad {
        """(def foo ()
          |  (if true 23 42))""".stripMargin
      }
      tc.foo() should be(23: JLong)
    }

    it("runs the false branch when false") {
      val tc = compileAndLoad {
        """(def foo ()
          |  (if false 23 42))""".stripMargin
      }
      tc.foo() should be(42: JLong)
    }

    it("can be used as an expr") {
      val tc = compileAndLoad {
        """(def foo (a)
          |  (+ (if a 23 42) 10))""".stripMargin
      }
      tc.foo(1: JLong) should be(33: JLong)
      tc.foo(0: JLong) should be(52: JLong)
    }
  }

  describe("codegen") {
    it("generates an if") {
      val ms = compile {
        """(def foo (a b)
          |  (if true a b))""".stripMargin
      }
      ms(s"foo($O$O)$O") shouldBe List(
        "LDC 1",
        "LCONST_0",
        "LCMP",
        "IFEQ L0",
        "ALOAD 0",
        "GOTO L1",
        "L0",
        "ALOAD 1",
        "L1",
        "ARETURN"
      )
    }

    it("can be used within other statements") {
      val ms = compile {
        """(def foo ()
          |  (+ 12 (if true 23 42)))""".stripMargin
      }
      ms(s"foo()$O") shouldBe List(
        "LDC 12",
        "LDC 1",
        "LCONST_0",
        "LCMP",
        "IFEQ L0",
        "LDC 23",
        "GOTO L1",
        "L0",
        "LDC 42",
        "L1",
        "LADD",
        box("J"),
        "ARETURN"
      )
    }

    it("unboxes test expression to long") {
      val ms = compile {
        """(def foo (a)
          |  (if a 23 42))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "LCONST_0",
        "LCMP",
        "IFEQ L0",
        "LDC 23",
        "GOTO L1",
        "L0",
        "LDC 42",
        "L1",
        box("J"),
        "ARETURN"
      )
    }

    describe("boxing") {
      it("boxes results if they're not Object") {
        val ms = compile {
          """(def foo (a)
            |  (if true a 23))""".stripMargin
        }
        ms(s"foo($O)$O") shouldBe List(
          "LDC 1",
          "LCONST_0",
          "LCMP",
          "IFEQ L0",
          "ALOAD 0",
          "GOTO L1",
          "L0",
          "LDC 23",
          box("J"),
          "L1",
          "ARETURN"
        )
      }

      it("doesn't box individually if both paths return the same type") {
        val ms = compile {
          """(def foo (a)
            |  (if true 23 42))""".stripMargin
        }
        ms(s"foo($O)$O") shouldBe List(
          "LDC 1",
          "LCONST_0",
          "LCMP",
          "IFEQ L0",
          "LDC 23",
          "GOTO L1",
          "L0",
          "LDC 42",
          "L1",
          box("J"),
          "ARETURN"
        )
      }

      it("figures out boxing requirements based on types") {
        val ms = compile {
          """(def foo (a)
            |  (+ 12 (if true 23 a)))""".stripMargin
        }
        ms(s"foo($O)$O") shouldBe List(
          "LDC 12",
          "LDC 1",
          "LCONST_0",
          "LCMP",
          "IFEQ L0",
          "LDC 23",
          box("J"),
          "GOTO L1",
          "L0",
          "ALOAD 0",
          "L1",
          checkCast("J"),
          unbox("J"),
          "LADD",
          box("J"),
          "ARETURN"
        )
      }
    }
  }
}
