import sbt._
import Keys._

object build extends Build {
    lazy val kenSettings = Project.defaultSettings ++ Seq(
        organization := "com.github.okomok",
        version := "0.1.0",

        scalaVersion := "2.9.1",
        crossScalaVersions := Seq("2.9.1", "2.9.2-RC1", "2.10.0-M2"),

        scalacOptions ++= Seq("-deprecation", "-unchecked"),
        scalacOptions <++= scalaVersion map { version =>
            // Thanks to Miles Sabin.
            val Version = """(\d+)\.(\d+)\..*"""r
            val Version(major0, minor0) = version map identity
            val (major, minor) = (major0.toInt, minor0.toInt)
            if (major < 2 || (major == 2 && minor < 10)) Seq("-Ydependent-method-types")
            else Nil
        },

        libraryDependencies ++= Seq(
            "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
            "junit" % "junit" % "4.4" % "test"
        ),

        parallelExecution := false,
        publishArtifact in packageDoc := false,

        publishTo := Some(Resolver.file("file", new java.io.File("../maven-repo/releases/")))
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
