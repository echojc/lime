package sh.echo.lime

class MainGenSpec extends GenBaseSpec {

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

  it("does not compile main methods if there are no free exprs") {
    val ms = compile {
      "(def foo () 1)"
    }
    ms should not contain key(javaMainDesc)
    ms should not contain key(limeMainDesc)
  }
}
