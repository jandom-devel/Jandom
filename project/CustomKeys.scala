import sbt._
import Keys._

object CustomKeys {
  val pplJar = settingKey[Option[String]]("Location of the PPL library")
  val gitHeadCommitSHA = taskKey[String]("Current git commit SHA")
}
