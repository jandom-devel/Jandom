import EclipseKeys._

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm-tree" % "4.1",
  "org.ow2.asm" % "asm-util" % "4.1",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scalanlp" %% "breeze-math" % "0.4",
  "org.rogach" %% "scallop" % "0.9.4",
  "soot" % "soot" % "2.5.0" from "http://www.sable.mcgill.ca/software/soot-2.5.0.jar",
  "org.scala-lang" % "scala-swing" % scalaVersion.value
)

unmanagedJars in Compile ++= (pplJar.value map file).toSeq

unmanagedSources in Compile := (unmanagedSources in Compile).value filter {
  source => pplJar.value.isDefined || ! source.getParent.endsWith("ppl")
}

unmanagedSources in Test := (unmanagedSources in Test).value filter {
  source => pplJar.value.isDefined || ! source.getParent.endsWith("ppl")
}


// Eclipse plugin

// It would be nice to be able to exclude resource directories from compilation.

createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource + EclipseCreateSrc.Managed

// BuildInfo plugin

buildInfoSettings

gitHeadCommitSHA := Process("git rev-parse HEAD").lines.head

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA)

buildInfoPackage := "it.unich.sci.jandom"

