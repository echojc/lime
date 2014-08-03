package sh.echo.exw

class ConsGenSpec extends GenBaseSpec {

  it("can create a simple list") {
    val ms = compile {
      """(defn foo ()
        |  (list 1 2))""".stripMargin
    }
    ms(s"foo()$O") shouldBe List(
      "NEW exw/Cons",
      "DUP",
      "LDC 1",
      box("J"),
      "NEW exw/Cons",
      "DUP",
      "LDC 2",
      box("J"),
      "INVOKESTATIC exw/Nil.get ()Lexw/List;",
      "INVOKESPECIAL exw/Cons.<init> (Ljava/lang/Object;Lexw/List;)V",
      "INVOKESPECIAL exw/Cons.<init> (Ljava/lang/Object;Lexw/List;)V",
      "ARETURN"
    )
  }

  it("can put variables into a list") {
    val ms = compile {
      """(defn foo (a b)
        |  (list 5 a b 6))""".stripMargin
    }
    ms(s"foo($O$O)$O") shouldBe List(
      "NEW exw/Cons",
      "DUP",
      "LDC 5",
      box("J"),
      "NEW exw/Cons",
      "DUP",
      "ALOAD 0",
      "NEW exw/Cons",
      "DUP",
      "ALOAD 1",
      "NEW exw/Cons",
      "DUP",
      "LDC 6",
      box("J"),
      "INVOKESTATIC exw/Nil.get ()Lexw/List;",
      "INVOKESPECIAL exw/Cons.<init> (Ljava/lang/Object;Lexw/List;)V",
      "INVOKESPECIAL exw/Cons.<init> (Ljava/lang/Object;Lexw/List;)V",
      "INVOKESPECIAL exw/Cons.<init> (Ljava/lang/Object;Lexw/List;)V",
      "INVOKESPECIAL exw/Cons.<init> (Ljava/lang/Object;Lexw/List;)V",
      "ARETURN"
    )
  }

  it("an empty list is empty") {
    val ms = compile {
      """(defn foo ()
        |  (list))""".stripMargin
    }
    ms(s"foo()$O") shouldBe List(
      "INVOKESTATIC exw/Nil.get ()Lexw/List;",
      "ARETURN"
    )
  }
}
