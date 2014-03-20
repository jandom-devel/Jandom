import EclipseKeys._

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm-tree" % "4.1",
  "org.ow2.asm" % "asm-util" % "4.1",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scalanlp" %% "breeze-math" % "0.4",
  "org.rogach" %% "scallop" % "0.9.4",
  "soot" % "soot" % "2.5.0+git1" from "http://vandyk.st.informatik.tu-darmstadt.de/abc/soot.jar",
  "org.scala-lang" % "scala-swing" % scalaVersion.value
)

unmanagedJars in Compile ++= (pplJar.value map file).toSeq

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

// Eclipse plugin

// It would be nice to be able to exclude resource directories from compilation.

createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

// BuildInfo plugin

buildInfoSettings

gitHeadCommitSHA := Process("git rev-parse HEAD").lines.head

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA)

buildInfoPackage := "it.unich.sci.jandom"

