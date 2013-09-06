import sbt._
import Keys._

object JandomBuild extends Build {
  val pplJar = settingKey[Option[String]]("Location of the PPL library")
  val gitHeadCommitSHA = taskKey[String]("Current git commit SHA")
}

