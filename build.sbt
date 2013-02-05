name := "pushpop"

organization := "org.michajlo"

version := "1.0"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation"

unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

