

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


import sbt._


class Ken(info: ProjectInfo) extends DefaultProject(info) {
    val scalaToolsSnapshotRepo = "scala-tools.org-SNAPSHOT" at "http://scala-tools.org/repo-snapshots/"

    val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"
    val junit = "junit" % "junit" % "4.4" % "test"

    override def compileOptions = super.compileOptions ++
        Seq(Deprecation, Unchecked/*, ExplainTypes*/)

    override def managedStyle = ManagedStyle.Maven
    override def pomExtra =
        <distributionManagement>
            <repository>
                <id>repo</id>
                <url>http://okomok.github.com/maven-repo/releases</url>
            </repository>
            <repository>
                <id>snapshot-repo</id>
                <url>http://okomok.github.com/maven-repo/snapshots</url>
            </repository>
        </distributionManagement>
    lazy val publishTo = Resolver.file("Publish", new java.io.File("../maven-repo/releases/"))
}