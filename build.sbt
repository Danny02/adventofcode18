name := "adventofcode"
version := "0.1"
scalaVersion := "2.12.7"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
