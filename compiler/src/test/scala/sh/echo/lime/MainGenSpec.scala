package sh.echo.lime

class MainGenSpec extends GenBaseSpec {

  describe("integration tests") {
    it("runs free statements as main") {
      val tc = compileAndLoad {
        """(put (+ 1 2))
          |(put (+ 3 4))""".stripMargin
      }
      val out = captureStdout {
        tc.main()
      }
      out shouldBe "3\n7\n"
    }

    it("provides access to args") {
      val tc = compileAndLoad {
        "(put (car (cdr args)))"
      }
      val out = captureStdout {
        tc.main("foo", "bar")
      }
      out shouldBe "bar\n"
    }

    it("calls functions from main") {
      val tc = compileAndLoad {
        """(def snd (lst)
          |  (car (cdr lst)))
          |(put (snd args))""".stripMargin
      }
      val out = captureStdout {
        tc.main("foo", "bar", "baz")
      }
      out shouldBe "bar\n"
    }
  }

  describe("codegen") {
    val javaMainDesc = s"main([Ljava/lang/String;)V"
    val limeMainDesc = s"main(Llime/List;)V"

    it("compiles free statements into the body of main") {
      val ms = compile {
        """(+ 1 2)
          |(+ 3 4)""".stripMargin
      }
      ms(limeMainDesc) shouldBe List(
        "LDC 1",
        "LDC 2",
        "LADD",
        "LDC 3",
        "LDC 4",
        "LADD",
        "RETURN"
      )
    }

    it("converts args to a cons list and calls the delegate main") {
      val ms = compile {
        "1"
      }
      ms(javaMainDesc) shouldBe List(
        "ALOAD 0",
        "INVOKESTATIC lime/Cons.fromArray ([Ljava/lang/Object;)Llime/List;",
        s"INVOKESTATIC $testUnit.main (Llime/List;)V",
        "RETURN"
      )
    }

    it("treats 'args' as a special variable in main") {
      val ms = compile {
        "(car args)"
      }
      ms(limeMainDesc) shouldBe List(
        "ALOAD 0",
        "CHECKCAST lime/List",
        "INVOKEINTERFACE lime/List.head ()Ljava/lang/Object;",
        "RETURN"
      )
    }

    it("does not compile main methods if there are no free exprs") {
      val ms = compile {
        "(def foo () 1)"
      }
      ms should not contain key(javaMainDesc)
      ms should not contain key(limeMainDesc)
    }
  }
}
