scalaVersion := "2.13.3"

name := "scalac-playground"
organization := "com.github.tanishiking"
version := "1.0.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"

// https://github.com/sbt/sbt/issues/2958#issuecomment-568496384
fork := true
