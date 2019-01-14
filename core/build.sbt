import CustomKeys._

//*** Libraries

libraryDependencies ++= Seq(
  "it.unich.scalafix" %% "scalafix" % "0.7.0",
  "org.apache.commons" % "commons-text" % "1.6",
  "org.typelevel" %% "spire" % "0.16.0",
  "org.rogach" %% "scallop" % "3.1.5",
  "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  // ASM is included in the Soot Jar
  "ca.mcgill.sable" % "soot" % "3.2.0",
  // SLF4J 1.7.5 is used in Soot
  "org.slf4j" % "slf4j-nop" % "1.7.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "org.mockito" % "mockito-core" % "2.23.4" % Test
)

//*** Add PPL, Apron, GMP jars

unmanagedJars in Compile ++= (pplJar.value map file).toSeq
unmanagedJars in Compile ++= (apronJar.value map file).toSeq
unmanagedJars in Compile ++= (gmpJar.value map file).toSeq

//*** Additional source directories for PPL

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

//*** Additional source directories for Apron

unmanagedSourceDirectories in Compile ++= (apronJar.value map { _ => (sourceDirectory in Compile).value / "apron" }).toSeq

unmanagedSourceDirectories in Test ++= (apronJar.value map { _ => (sourceDirectory in Test).value / "apron" }).toSeq

//*** BuildInfo plugin

buildInfoKeys ++= Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA)

buildInfoPackage := "it.unich.jandom"

//*** IDEA plugin

ideOutputDirectory in Compile := Some(file("core/target/idea/classes"))

ideOutputDirectory in Test := Some(file("core/target/idea/test-classes"))

