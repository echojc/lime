
scalaVersion := "2.11.1"

name := "lime-stdlib"

organization := "sh.echo"

version := "0.0.1"

crossPaths := false

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

scalacOptions := Seq(
  "-deprecation",
  "-feature"
)
