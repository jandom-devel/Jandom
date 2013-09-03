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

val pplJar = Process("ppl-config -l").lines.head+"/ppl/ppl_java.jar"

unmanagedJars in Compile += file(pplJar)

// Eclipse

classpathTransformerFactories += NativeLibTransformerFactory(pplJar)

// add resources to classpath. We need a plugin which allows to
// exclude resource directories from compilation.

createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource + EclipseCreateSrc.Managed

// BuildInfo

buildInfoSettings

val gitHeadCommitSHA = TaskKey[String]("git-head-commit-sha","current git commit SHA")

gitHeadCommitSHA := Process("git rev-parse HEAD").lines.head

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA)

buildInfoPackage := "it.unich.sci.jandom"

