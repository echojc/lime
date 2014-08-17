package sh.echo.lime

class ConsGenSpec extends GenBaseSpec {

  describe("integration tests") {
    it("makes lists") {
      val tc = compileAndLoad {
        """(def foo (a b)
          |  '(a b))""".stripMargin
      }
      tc.foo(1: JLong, 2: JLong) should be(Cons(1: JLong, 2: JLong))
    }

    it("can cons onto lists") {
      val tc = compileAndLoad {
        """(def foo (a b)
          |  (cons a b))""".stripMargin
      }
      tc.foo(1: JLong, Cons(2: JLong)) should be(Cons(1: JLong, 2: JLong))
    }

    it("cars") {
      val tc = compileAndLoad {
        """(def foo (a)
          |  (car a))""".stripMargin
      }
      tc.foo(Cons(1: JLong, 2: JLong)) should be(1: JLong)
    }

    it("throws an exception when you car nil") {
      val tc = compileAndLoad {
        """(def foo ()
          |  (car nil))""".stripMargin
      }
      intercept[Exception] {
        tc.foo()
      }
    }

    it("cdrs") {
      val tc = compileAndLoad {
        """(def foo (a)
          |  (cdr a))""".stripMargin
      }
      tc.foo(Cons(1: JLong, 2: JLong)) should be(Cons(2: JLong))
    }

    it("throws an exception when you cdr nil") {
      val tc = compileAndLoad {
        """(def foo ()
          |  (cdr nil))""".stripMargin
      }
      intercept[Exception] {
        tc.foo()
      }
    }

    describe("empty") {
      it("returns number '1' when empty") {
        val tc = compileAndLoad {
          """(def foo (a)
            |  (empty a))""".stripMargin
        }
        tc.foo(Cons()) should be (1: JLong)
      }

      it("returns number '0' when not empty") {
        val tc = compileAndLoad {
          """(def foo (a)
            |  (empty a))""".stripMargin
        }
        tc.foo(Cons("a")) should be (0: JLong)
      }

      it("can be used in a true if statement") {
        val tc = compileAndLoad {
          """(def foo (a)
            |  (if (empty a) "t" "f"))""".stripMargin
        }
        tc.foo(Cons()) shouldBe "t"
      }

      it("can be used in a false if statement") {
        val tc = compileAndLoad {
          """(def foo (a)
            |  (if (empty a) "t" "f"))""".stripMargin
        }
        tc.foo(Cons("a")) shouldBe "f"
      }
    }
  }

  describe("basic operations") {
    it("conses") {
      val ms = compile {
        """(def foo (a b)
          |  (cons a b))""".stripMargin
      }
      ms(s"foo($O$O)$O") shouldBe List(
        "NEW lime/Cons",
        "DUP",
        "ALOAD 0",
        "ALOAD 1",
        "CHECKCAST lime/List",
        "INVOKESPECIAL lime/Cons.<init> (Ljava/lang/Object;Llime/List;)V",
        "ARETURN"
      )
    }

    it("cars") {
      val ms = compile {
        """(def foo (a)
          |  (car a))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        "CHECKCAST lime/List",
        "INVOKEINTERFACE lime/List.head ()Ljava/lang/Object;",
        "ARETURN"
      )
    }

    it("cdrs") {
      val ms = compile {
        """(def foo (a)
          |  (cdr a))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        "CHECKCAST lime/List",
        "INVOKEINTERFACE lime/List.tail ()Llime/List;",
        "ARETURN"
      )
    }

    it("can check for empty lists") {
      val ms = compile {
        """(def foo (a)
          |  (empty a))""".stripMargin
      }
      ms(s"foo($O)$O") shouldBe List(
        "ALOAD 0",
        "INSTANCEOF lime/Nil",
        "I2L",
        box("J"),
        "ARETURN"
      )
    }
  }

  describe("creation") {
    it("can create a simple list") {
      val ms = compile {
        """(def foo ()
          |  (list 1 2))""".stripMargin
      }
      ms(s"foo()$O") shouldBe List(
        "NEW lime/Cons",
        "DUP",
        "LDC 1",
        box("J"),
        "NEW lime/Cons",
        "DUP",
        "LDC 2",
        box("J"),
        "INVOKESTATIC lime/Nil.get ()Llime/List;",
        "INVOKESPECIAL lime/Cons.<init> (Ljava/lang/Object;Llime/List;)V",
        "INVOKESPECIAL lime/Cons.<init> (Ljava/lang/Object;Llime/List;)V",
        "ARETURN"
      )
    }

    it("can put variables into a list") {
      val ms = compile {
        """(def foo (a b)
          |  (list 5 a b 6))""".stripMargin
      }
      ms(s"foo($O$O)$O") shouldBe List(
        "NEW lime/Cons",
        "DUP",
        "LDC 5",
        box("J"),
        "NEW lime/Cons",
        "DUP",
        "ALOAD 0",
        "NEW lime/Cons",
        "DUP",
        "ALOAD 1",
        "NEW lime/Cons",
        "DUP",
        "LDC 6",
        box("J"),
        "INVOKESTATIC lime/Nil.get ()Llime/List;",
        "INVOKESPECIAL lime/Cons.<init> (Ljava/lang/Object;Llime/List;)V",
        "INVOKESPECIAL lime/Cons.<init> (Ljava/lang/Object;Llime/List;)V",
        "INVOKESPECIAL lime/Cons.<init> (Ljava/lang/Object;Llime/List;)V",
        "INVOKESPECIAL lime/Cons.<init> (Ljava/lang/Object;Llime/List;)V",
        "ARETURN"
      )
    }

    it("an empty list is empty") {
      val ms = compile {
        """(def foo ()
          |  (list))""".stripMargin
      }
      ms(s"foo()$O") shouldBe List(
        "INVOKESTATIC lime/Nil.get ()Llime/List;",
        "ARETURN"
      )
    }
  }
}
