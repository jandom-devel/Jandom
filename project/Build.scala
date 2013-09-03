import sbt._
import sbt.Keys._

object JandomBuild extends Build {
  val gitHeadCommitSHA = TaskKey[String]("git-head-commit-sha","current git commit SHA")

  lazy val root = (
    Project("root", file("."))
    aggregate(core,extended)
  )

  lazy val core = (
    Project("Jandom", file("core"))
  )

  lazy val extended = (
    Project("JandomExtended", file("extended"))
    dependsOn(core)
  )
}

