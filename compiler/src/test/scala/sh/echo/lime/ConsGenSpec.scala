package sh.echo.lime

class ConsGenSpec extends GenBaseSpec {

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
