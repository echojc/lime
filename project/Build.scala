import sbt._
import Keys._

object HelloBuild extends Build {
  lazy val compiler = (
    project in file("compiler")
      dependsOn stdlib
  )
  lazy val stdlib = (
    project in file("stdlib")
  )
}
