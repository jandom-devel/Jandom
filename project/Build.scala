import sbt._
import sbt.Keys._

object JandomBuild extends Build {
  lazy val Benchmark = config("benchmark") extend (Compile) describedAs ("Configuration for Benchmarks.")

  val gitHeadCommitSHA = TaskKey[String]("git-head-commit-sha","current git commit SHA")

  lazy val root = (
    Project("root", file("."))
    aggregate(core,extended)
    configs(Benchmark)   // only needed so that we may refer the Benchmark config globally
  )

  lazy val core = (
    Project("Jandom", file("core"))
    configs(Benchmark)  // only needed so that we may refer the Benchmark config globally
  )

  lazy val extended = (
    Project("JandomExtended", file("extended"))
    dependsOn(core)
    configs(Benchmark)
    settings( 
      Project.defaultSettings ++
      inConfig(Benchmark)(Defaults.testSettings) ++
      CaliperPlugin.benchmarkTasks :_*
     ) 
   )
}

