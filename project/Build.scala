import sbt._
import sbt.Keys._

object JandomBuild extends Build {
  lazy val Benchmark = config("benchmark") extend (Compile) describedAs ("Configuration for Benchmarks.")

  lazy val root =
    Project(id="root", base = file("."))
    .aggregate(core,extended)
    .configs(Benchmark)   // only needed so that we may refer the Benchmark config globally

  lazy val core =
    Project(id = "Jandom", base = file("core"))
    .configs(Benchmark)  // only needed so that we may refer the Benchmark config globally

  lazy val extended =
    Project(id = "JandomExtended", base = file("extended"))
    .dependsOn(core)
    .configs(Benchmark)
    .settings(
      Project.defaultSettings ++
      inConfig(Benchmark)(Defaults.testSettings) ++
      CaliperPlugin.benchmarkTasks :_*
     ) 
}

