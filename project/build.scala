import sbt._
import Keys._

object build extends Build {
    lazy val ken = Project(
        id = "ken",
        base = file("."),
        aggregate = Seq(core, enumerator, parsec, quickcheck)
    )

    lazy val core = Project(
        id = "ken-core",
        base = file("core")
    )

    lazy val enumerator = Project(
        id = "ken-enumerator",
        base = file("enumerator"),
        dependencies = Seq(core)
    )

    lazy val parsec = Project(
        id = "ken-parsec",
        base = file("parsec"),
        dependencies = Seq(core)
    )

    lazy val quickcheck = Project(
        id = "ken-quickcheck",
        base = file("quickcheck"),
        dependencies = Seq(core)
    )
}
