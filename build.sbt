name := "codegrader"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.7.1"

// https://mvnrepository.com/artifact/org.junit.jupiter/junit-jupiter-api
libraryDependencies += "org.junit.jupiter" % "junit-jupiter-api" % "5.3.1"

// https://alvinalexander.com/scala/how-to-use-junit-testing-with-scala
libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

enablePlugins(Antlr4Plugin)
antlr4PackageName in Antlr4 := Some("com.ethanshea.codegrader")

// Enable when we need them
antlr4GenListener in Antlr4 := true
antlr4GenVisitor in Antlr4 := true