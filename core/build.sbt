//*** Libraries

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
  "org.mockito" % "mockito-core" % "1.10.19" % "test",
  "org.scalanlp" %% "breeze" % "0.11.2",
  // for using native linear algebra libraries
  // "org.scalanlp" %% "breeze-natives" % "0.9",
  "org.rogach" %% "scallop" % "0.9.5",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  // ASM is included in the Soot Jar
  "soot" % "soot" % "2.5.0+git2" from "https://ssebuild.cased.de/nightly/soot/lib/soot-trunk.jar"
)

//*** Additional source directories for PPL

unmanagedJars in Compile ++= (pplJar.value map file).toSeq

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

//*** Eclipse plugin

EclipseKeys.createSrc := EclipseCreateSrc.Default +  EclipseCreateSrc.Managed

EclipseKeys.classpathTransformerFactories := Seq(ClasspathentryTransformer)

// It would be nice to be able to exclude resource directories from compilation.

managedSourceDirectories in Test := Seq()

managedResourceDirectories in Test := Seq()

managedResourceDirectories in Compile := Seq()

unmanagedResourceDirectories in Compile := Seq()

//*** BuildInfo plugin

gitHeadCommitSHA := Process("git rev-parse HEAD").lines.head

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA)

buildInfoPackage := "it.unich.jandom"

