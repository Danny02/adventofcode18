ThisBuild / name := "adventofcode"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.12.7"

lazy val day1 = project
lazy val day2 = project
lazy val day3 = project

day3 / libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.0.4"