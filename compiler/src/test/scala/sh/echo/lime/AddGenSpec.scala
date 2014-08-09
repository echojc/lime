package sh.echo.lime

class AddGenSpec extends GenBaseSpec {

  describe("integration tests") {
    it("adds") {
      val tc = compileAndLoad {
        """(def foo (a)
          |  (+ a 23))""".stripMargin
      }
      tc.foo(42: JLong) should be(65: JLong)
    }
  }

  describe("basic operations") {
    it("compiles a simple addition function") {
      val ms = compile {
        """(def foo (a)
          |  (+ a 23))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "LDC 23",
        "LADD",
        box("J"),
        "ARETURN"
      )
    }

    it("compiles nested addition functions") {
      val ms = compile {
        """(def foo (a)
          |  (+ a (+ (+ 23 a) 42)))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "LDC 23",
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "LADD",
        "LDC 42",
        "LADD",
        "LADD",
        box("J"),
        "ARETURN"
      )
    }

    it("compiles a varargs addition function") {
      val ms = compile {
        """(def foo (a b)
          |  (+ a 23 b 42))""".stripMargin
      }
      ms(s"foo($O$O)$O") shouldBe List(
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "LDC 23",
        "ALOAD 1",
        checkCast("J"),
        unbox("J"),
        "LDC 42",
        "LADD",
        box("J"),
        "ARETURN"
      )
    }

    it("compiles a multi-argument function") {
      val ms = compile {
        """(def foo (a b)
          |  (+ a b))""".stripMargin
      }
      ms(s"foo($O$O)$O") shouldBe List(
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "ALOAD 1",
        checkCast("J"),
        unbox("J"),
        "LADD",
        box("J"),
        "ARETURN"
      )
    }

    it("loads arguments in the specified order function") {
      val ms = compile {
        """(def foo (a b)
          |  (+ b a))""".stripMargin
      }
      ms(s"foo($O$O)$O") shouldBe List(
        "ALOAD 1",
        checkCast("J"),
        unbox("J"),
        "ALOAD 0",
        checkCast("J"),
        unbox("J"),
        "LADD",
        box("J"),
        "ARETURN"
      )
    }
  }
}
