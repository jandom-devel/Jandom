// general data

name := "Jandom"

version := "0.1.2"

scalaVersion := "2.10.0"

// depedendency management

resolvers ++= Seq(
   // this resolver is needed for the breeze snapshot
   "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test,benchmark",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
  "org.rogach" %% "scallop" % "0.8.0",
  "com.google.caliper" % "caliper" % "0.5-rc1" % "benchmark"
)

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

// assembly plugin configuration

assemblySettings

test in AssemblyKeys.assembly := {}  // skip tests in assembly

// eclipse-sbt plugin configuration

EclipseKeys.configurations := Set(Compile, Test, Benchmark)  

// PPL setup

fork  := true

unmanagedJars in Compile += file("/usr/local/lib/ppl/ppl_java.jar")

javaOptions += "-Djava.library.path=/usr/local/lib/ppl"

EclipseKeys.classpathTransformerFactories += NativeLibTransformerFactory("/usr/local/lib/ppl/ppl_java.jar") 
