
scalaVersion := "2.11.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "org.ow2.asm" % "asm" % "5.0.3",
  "org.ow2.asm" % "asm-util" % "5.0.3",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

scalacOptions := Seq(
  "-deprecation",
  "-feature"
)
