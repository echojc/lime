package sh.echo.lime

class IfGenSpec extends GenBaseSpec {

  it("generates an if") {
    val ms = compile {
      """(def foo (a b)
        |  (if true a b))""".stripMargin
    }
    ms(s"foo($O$O)$O") shouldBe List(
      "LDC 1",
      "IFNE L0",
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
      "IFNE L0",
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

  describe("boxing") {
    it("boxes results if they're not Object") {
      val ms = compile {
        """(def foo (a)
          |  (if true a 23))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "LDC 1",
        "IFNE L0",
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
        "IFNE L0",
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
        "IFNE L0",
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
