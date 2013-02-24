import sbt._
import sbt.Keys._

object JandomBuild extends Build {
  lazy val Benchmark = config("benchmark") extend (Compile) describedAs ("Configuration for Benchmarks.")

  lazy val root =
    Project(id="root", base = file("."))
    .aggregate(core,macros)

  lazy val core =
    Project(id = "Jandom", base = file("core"))
      .configs(Benchmark)
      .settings(
        inConfig(Benchmark)(Defaults.testSettings) ++
        CaliperPlugin.benchmarkTasks :_*
      )

  lazy val macros =
    Project(id = "JandomExtended", base = file("extended"))
    .dependsOn(core)
}
