import sbt._
import sbt.Keys._

object Jandom extends Build {
  lazy val Benchmark = config("benchmark") extend (Compile) describedAs ("Configuration for Benchmarks.")
  
  lazy val root =
    Project(id = "root", base = file("."))
      .configs(Benchmark)
      .settings(inConfig(Benchmark)(Defaults.testSettings): _*)
 
}
