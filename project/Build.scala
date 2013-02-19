import sbt._
import sbt.Keys._

object Jandom extends Build {
  val benchmark = TaskKey[Unit]("benchmark", "Executes all benchmarks.")

  lazy val Benchmark = config("benchmark") extend (Compile) describedAs ("Configuration for Benchmarks.")

  lazy val root =
    Project(id = "root", base = file("."))
      .configs(Benchmark)
      .settings(inConfig(Benchmark)(Defaults.testSettings): _*)
      .settings(benchmark <<= test in Benchmark)
}
