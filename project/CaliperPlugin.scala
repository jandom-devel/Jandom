import sbt._
import sbt.Keys._

/**
 * This is a plugin for using Caliper benchmarks within sbt. It is based upon
 * [[https://github.com/alno/sbt-caliper sbt-caliper]] with two differences:
 *  1. there is a new configuration called Benchmark
 *  1. the task `benchmark` looks for all classes in the Jandom.Benchmark 
 * configuration whose names ends in Benchmark, and executes them with
 * [[http://code.google.com/p/caliper/ Caliper]].
 * 
 * With respect to point 2, I am not satisfied. I would like to control the
 * configurationt to use with a key, but I am not able to do this. Perahps
 * in sbt 0.13 with the use of the value method.
 */

object CaliperPlugin extends sbt.Plugin {
 
  val benchmark = TaskKey[Unit]("benchmark", "Executes all benchmarks.")
  val benchmarkOnly = InputKey[Unit]("benchmark-only", "Executes specified benchmarks.")
   
  lazy val benchmarkTasks = Seq(
    benchmark <<= benchmarkTaskInit.zip(classDirectory in benchmarkConfig) {
      case (runTask, classdirs) =>
        (runTask :^: KNil) map {
          case run :+: HNil =>
            val benchfiles = (classdirs ** "*Benchmark.class").get
            val paths = benchfiles map { _.getParentFile } map { relativeTo(classdirs) }
            val names = benchfiles map { _.base }
            val classes = for ((Some(path), name) <- paths zip names)
              yield path.replace(java.io.File.separatorChar, '.') + '.' + name
            run { classes }
        }
    },

    benchmarkOnly <<= sbt.inputTask { (argTask: TaskKey[Seq[String]]) =>
      benchmarkTaskInit.zip(argTask) {
        case (runTask, argTask) =>
          (runTask :^: argTask :^: KNil) map {
            case run :+: args :+: HNil =>
              run { args }
          }
      }
    })

  override val settings = benchmarkTasks

  private def benchmarkTaskInit: Project.Initialize[Task[Seq[String] => Unit]] =
    (fullClasspath in benchmarkConfig, scalaInstance, javaHome, javaOptions, baseDirectory, outputStrategy, streams) map {
      (cpa, si, jhome, jopts, dir, strategy, s) =>
        val cp = "-classpath" :: Path.makeString(cpa.files) :: Nil
        val fr = new ForkRun(
          ForkOptions(scalaJars = si.jars,
            javaHome = jhome,
            runJVMOptions = jopts ++ cp,
            outputStrategy = strategy,
            workingDirectory = Some(dir)))

        { args: Seq[String] =>
          if (args.isEmpty)
            println("No benchmarks specified - nothing to run")
          else
            sbt.toError(fr.run("com.google.caliper.Runner", Build.data(cpa), args, s.log))
        }
    }
}

