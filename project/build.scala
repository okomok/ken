import sbt._
import Keys._
import Project.Setting

object build extends Build {
    type Sett = Project.Setting[_]

    lazy val kenSettings: Seq[Sett] = Defaults.defaultSettings ++ Seq[Sett](
        organization := "com.github.okomok",
        version := "0.1.0-SNAPSHOT",
        scalaVersion := "2.9.1",
        scalacOptions ++= Seq("-deprecation", "-unchecked"/*, "-Ydependent-method-types"*/),
        libraryDependencies ++= Seq(
            "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
            "junit" % "junit" % "4.4" % "test"
        ),
        parallelExecution in Test := false
    )

    lazy val ken = Project(
        id = "ken",
        base = file("."),
        settings = kenSettings
    )

    lazy val enumerator = Project(
        id = "ken-enumerator",
        base = file("enumerator"),
        settings = kenSettings,
        dependencies = Seq(ken)
    )

    lazy val parsec = Project(
        id = "ken-parsec",
        base = file("parsec"),
        settings = kenSettings,
        dependencies = Seq(ken)
    )

    lazy val quickcheck = Project(
        id = "ken-quickcheck",
        base = file("quickcheck"),
        settings = kenSettings,
        dependencies = Seq(ken)
    )

    lazy val kenAggregate = Project(
        id = "ken-aggregate",
        base = file("aggregate"),
        settings = kenSettings,
        aggregate = Seq(ken, enumerator, parsec, quickcheck)
    )
}