import CustomKeys._

//*** Metadata for the build

ThisBuild / version := "0.1.3-SNAPSHOT"
ThisBuild / description := "A static analyzer based on abstract interpretation"
ThisBuild / organization := "it.unich.jandom"
ThisBuild / licenses := Seq("LGPL-3.0" -> url("https://opensource.org/licenses/LGPL-3.0"))
ThisBuild / homepage:= Some(url("https://github.com/jandom-devel/Jandom"))
ThisBuild / startYear := Some(2011)
ThisBuild / developers := List(
  Developer(
    "amato",
    "Gianluca Amato", "gianluca.amato@unich.it",
    url("http://www.sci.unich.it/~amato/")
  ),
  Developer(
    "scozzari",
    "Francesca Scozzari", "francesca.scozzari@unich.it",
    url("http://www.sci.unich.it/~scozzari/")
  )
)
ThisBuild / scmInfo := Some(ScmInfo(
  url("https://github.com/jandom-devel/Jandom"),
  "scm:git:https://github.com/jandom-devel/Jandom.git",
  Some("scm:git:https://github.com/jandom-devel/Jandom.git")
))

//*** Scala configuration

ThisBuild / scalaVersion := "2.12.15"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint:_,-missing-interpolator",
  "-Ywarn-unused:-implicits"
)

//*** Build system configuration

ThisBuild / fork := true
ThisBuild / resolvers ++= Seq (
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Soot snapshot" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-snapshot/",
  "Soot release" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-release/"
)

//*** Custom keys

ThisBuild / pplJar := {
  try {
    val PPLPathName = scala.sys.process.Process("ppl-config -l").lineStream.head+"/ppl/ppl_java.jar"
    if (file(PPLPathName).exists) Some(PPLPathName) else None
  } catch {
    case _ : Exception => None
  }
}
ThisBuild / apronJar := {
  try {
    val ApronPathName = "/usr/share/java/apron.jar"
    // TODO Parameterize
    if (file(ApronPathName).exists) Some(ApronPathName) else None
  } catch {
    case _ : Exception => None
  }
}
ThisBuild / gmpJar := {
  try {
    val GMPPathName = "/usr/share/java/gmp.jar"
    // TODO Parameterize
    if (file(GMPPathName).exists) Some(GMPPathName) else None
  } catch {
    case _ : Exception => None
  }
}
ThisBuild / gitHeadCommitSHA := {
  scala.sys.process.Process("git rev-parse HEAD").lineStream.head
}

lazy val additionalSources = Seq(
     //*** Additional source directories for PPL
    Compile / unmanagedSourceDirectories ++= pplJar.value.toSeq map { _ => (Compile / sourceDirectory).value / "ppl" },
    Test / unmanagedSourceDirectories ++= pplJar.value.toSeq map { _ => (Test / sourceDirectory).value / "ppl" },

    //*** Additional source directories for Apron
    Compile / unmanagedSourceDirectories ++= apronJar.value.toSeq map { _ => (Compile / sourceDirectory).value / "apron" },
    Test / unmanagedSourceDirectories ++= apronJar.value.toSeq map { _ => (Test / sourceDirectory).value / "apron" }
)

//*** Declare projects

lazy val jandom = (project in file("."))
  .aggregate(core, extended) 

lazy val core = project
  .enablePlugins(BuildInfoPlugin)
  .settings(
    additionalSources,

    //*** Library dependencies
    libraryDependencies ++= Seq(
      "it.unich.scalafix" %% "scalafix" % "0.7.0",
      "org.apache.commons" % "commons-text" % "1.9",
      "org.typelevel" %% "spire" % "0.17.0",
      "org.rogach" %% "scallop" % "4.1.0",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      // ASM is included in the Soot Jar
      "ca.mcgill.sable" % "soot" % "4.1.0",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.11" % Test,
      "org.scalatest" %% "scalatest-funspec" % "3.2.11" % Test,
      "org.scalatest" %% "scalatest-propspec" % "3.2.11" % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test,
      "org.scalatestplus" %% "mockito-4-2" % "3.2.11.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
      "org.mockito" % "mockito-core" % "4.4.0" % Test
    ),

    //*** Add PPL, Apron, GMP jars
    Compile / unmanagedJars ++= pplJar.value.toSeq map file,
    Compile / unmanagedJars ++= apronJar.value.toSeq map file,
    Compile / unmanagedJars ++= gmpJar.value.toSeq map file,

    //*** BuildInfo plugin
    buildInfoKeys ++= Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitHeadCommitSHA),
    buildInfoPackage := "it.unich.jandom",

    //*** IDEA plugin
    Compile / ideOutputDirectory := Some(file("core/target/idea/classes")),
    Test / ideOutputDirectory := Some(file("core/target/idea/test-classes")),
  )

lazy val extended = project
  .dependsOn(core % "compile->compile;test->test")
  .enablePlugins(JmhPlugin)
  .settings(
    additionalSources,

    //*** IDEA plugin
    Compile / ideOutputDirectory := Some(file("extended/target/idea/classes")),
    Test / ideOutputDirectory := Some(file("extended/target/idea/test-classes")),

    //*** JMH Plugin
    Jmh / dependencyClasspath ++= (Test / exportedProducts).value,

    assembly / assemblyJarName := "jandom-" + version.value + ".jar",
    assembly / assemblyOutputPath := (ThisBuild / baseDirectory).value / (assembly / assemblyJarName).value
  )


//** Do not warn for unused keys

Global / excludeLintKeys += ideOutputDirectory

//** Configure the assembly plugin

assembly / aggregate := false
assembly := (extended / assembly).value
ThisBuild / assembly / assemblyMergeStrategy := {
    case PathList(ps @ _*) if ps.last endsWith ".class"  => MergeStrategy.last
    case x => 
      val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
      oldStrategy(x)
}

//*** This delegates the Jandom run task to execute the run task in the Jandom sub-projects

run := (core / Compile / run).evaluated
Test / run := (core / Test / run).evaluated
Jmh / run := (extended / Jmh / run).evaluated
