import sbt._
import Keys._

object build extends Build {
    lazy val kenSettings = Project.defaultSettings ++ Seq(
        organization := "com.github.okomok",
        version := "0.1.0-SNAPSHOT",
        scalaVersion := "2.9.1",
        crossScalaVersions := Seq("2.9.1", "2.10.0-SNAPSHOT"),
        scalacOptions ++= Seq("-deprecation", "-unchecked"/*, "-Ydependent-method-types"*/),
        libraryDependencies ++= Seq(
            "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
            "junit" % "junit" % "4.4" % "test"
        ),
        resolvers += ScalaToolsSnapshots,
        parallelExecution := false,
        publishArtifact in packageDoc := false
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

    lazy val all = Project(
        id = "all",
        base = file("all"),
        settings = kenSettings ++ Seq(
            publishArtifact := false,
            publish := (),
            publishLocal := ()
        ),
        aggregate = Seq(ken, enumerator, parsec, quickcheck)
    )
}
