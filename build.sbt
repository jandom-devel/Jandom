// versions

version in ThisBuild := "0.1.2"

scalaVersion in ThisBuild := "2.10.1"

// task which refers to tasks in the subprojects 

run <<= run in ("Jandom", Compile)

// depedendency management

resolvers in ThisBuild ++= Seq(
   // this resolver is needed for the breeze snapshot
   "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies in ThisBuild  ++= Seq(
  "org.ow2.asm" % "asm-tree" % "4.1",
  "org.ow2.asm" % "asm-util" % "4.1",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test,benchmark",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
  "org.rogach" %% "scallop" % "0.8.0",
  "com.google.caliper" % "caliper" % "0.5-rc1" % "benchmark"
)

libraryDependencies in ThisBuild <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

// PPL setup for the entire build

fork in ThisBuild := true

javaOptions in ThisBuild += "-Djava.library.path=/usr/local/lib/ppl"

