import CustomKeys._

//*** Libraries

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.6",
  "org.scalatest" %% "scalatest" % "3.0.4" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
  "org.mockito" % "mockito-core" % "2.10.0" % Test,
  "org.typelevel" %% "spire" % "0.14.1",
  "it.unich.scalafix" %% "scalafix" % "0.7.0-SNAPSHOT",
  "org.rogach" %% "scallop" % "3.1.0",
  "org.scala-lang.modules" %% "scala-swing" % "2.0.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  // ASM is included in the Soot Jar
  "ca.mcgill.sable" % "soot" %"3.0.0-SNAPSHOT"
)

//*** Additional source directories for PPL

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

unmanagedJars in Compile ++= (pplJar.value map file).toSeq

//*** BuildInfo plugin

buildInfoKeys ++= Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA)

buildInfoPackage := "it.unich.jandom"

//*** IDEA plugin

ideOutputDirectory in Compile := Some(file("core/target/idea/classes"))

ideOutputDirectory in Test := Some(file("core/target/idea/test-classes"))

