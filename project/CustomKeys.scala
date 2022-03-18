import sbt._

object CustomKeys {
  val apronJar = settingKey[Option[String]]("Location of the Apron library")
  val gmpJar = settingKey[Option[String]]("Location of the GMP library")
  val pplJar = settingKey[Option[String]]("Location of the PPL library")
  val gitHeadCommitSHA = taskKey[String]("Current git commit SHA")
}
