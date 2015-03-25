import EclipseKeys._

val Jandom = project in file("core")

val JandomExtended = project in file("extended") dependsOn Jandom % "compile->compile;test->test"

// Add a new benchmark configuration...
// val Bench = config("bench") extend(Test)
// val root = project in file(".") configs(Bench) settings( inConfig(Bench) (Defaults.testSettings):_*) aggregate (Jandom, JandomExtended) 

val root = project in file(".") aggregate (Jandom, JandomExtended) 

version in ThisBuild := "0.1.3-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.4"

fork in ThisBuild := true

executionEnvironment in ThisBuild := Some(EclipseExecutionEnvironment.JavaSE17)

eclipseOutput in ThisBuild := Some("target.eclipse")

// for removing warnings when Breeze does not find native libraries
//
// javaOptions in ThisBuild ++= Seq("-Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.F2jBLAS",
//   "-Dcom.github.fommil.netlib.LAPACK=com.github.fommil.netlib.F2jLAPACK",
//   "-Dcom.github.fommil.netlib.ARPACK=com.github.fommil.netlib.F2jARPACK")

val optionalPPLPathName = try {
    val PPLPathName = Process("ppl-config -l").lines.head+"/ppl/ppl_java.jar"
    if (file(PPLPathName).exists) Some(PPLPathName) else None
  } catch {
    case _ : Exception => None 
  }

pplJar in ThisBuild := optionalPPLPathName

// This delegates the root run task to the run task in JandomCore

run <<= run in ("Jandom", Compile)

run in Test <<= run in ("Jandom", Test)

