
organization := "com.github.okomok"

name := "ken"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.1"

resolvers += ScalaToolsSnapshots

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
    "junit" % "junit" % "4.4" % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Ydependent-method-types")
