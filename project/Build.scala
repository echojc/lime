import sbt._
import Keys._

object HelloBuild extends Build {
  lazy val compiler = (
    project in file("compiler")
  )
  lazy val stdlib = (
    project in file("stdlib")
      dependsOn compiler
  )
}
