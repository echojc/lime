package sh.echo.lime

import java.io.File
import java.io.FileOutputStream
import scala.io.Source

object Main extends App {
  println("lime compiler 0.0.0pre-alpha")
  println("https://github.com/echojc/lime")
  println()

  // these should be objects or something
  val lp = new LispParser
  val cg = new ClassGen

  args foreach { path ⇒
    print(s"Compiling $path... ")

    val file = new File(path)
    if (file.exists()) {
      val code = Source.fromFile(file).mkString
      val name = path.split("/").last.split("\\.").dropRight(1).mkString(".")
      val bytecode = cg.compileUnit(name, lp.parse(code))

      val outPath = ((path.split("\\.").dropRight(1)) :+ "class").mkString(".")
      val fos = new FileOutputStream(outPath)
      fos.write(bytecode)
      fos.close()
      print(s"compiled successfully to $outPath")
    } else {
      print("file not found!")
    }

    println()
  }
}
