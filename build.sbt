name := "pushpop"

organization := "org.michajlo"

version := "1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation", "-unchecked")

unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

