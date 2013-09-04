import sbt._
import Keys._

object JandomBuild extends Build {
  val pplJar = taskKey[Option[String]]("Determine the location of the PPL library")
  val gitHeadCommitSHA = TaskKey[String]("git-head-commit-sha","current git commit SHA")
}

