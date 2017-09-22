import CustomKeys._

//*** Libraries

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.5",
  "org.scalatest" %% "scalatest" % "3.0.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.3" % Test,
  "org.mockito" % "mockito-core" % "2.2.9" % Test,
  "org.spire-math" %% "spire" % "0.12.0",
  "it.unich.scalafix" %% "scalafix" % "0.6.0",
  "org.rogach" %% "scallop" % "2.0.3",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
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

