import EclipseKeys._

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm-tree" % "4.1",
  "org.ow2.asm" % "asm-util" % "4.1",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "org.scalanlp" %% "breeze" % "0.9",
  // for using native linear algebra libraries
  // "org.scalanlp" %% "breeze-natives" % "0.9",
  "org.rogach" %% "scallop" % "0.9.5",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "soot" % "soot" % "2.5.0+git1" from "http://vandyk.st.informatik.tu-darmstadt.de/abc/soot.jar"
)

unmanagedJars in Compile ++= (pplJar.value map file).toSeq

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

// Eclipse plugin

// It would be nice to be able to exclude resource directories from compilation.

createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

managedSourceDirectories in Test := Seq()

// BuildInfo plugin

buildInfoSettings

gitHeadCommitSHA := Process("git rev-parse HEAD").lines.head

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA)

buildInfoPackage := "it.unich.jandom"

