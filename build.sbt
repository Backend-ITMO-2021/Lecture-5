name := "Lecture-5"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
mainClass in(Compile, run) := Some("ru.ifmo.backend_2021.Sudoku")