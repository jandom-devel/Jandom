import CustomKeys._

//*** Additional source directories for PPL

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

//*** Additional source directories for Apron

unmanagedSourceDirectories in Compile ++= (apronJar.value map { _ => (sourceDirectory in Compile).value / "apron" }).toSeq

unmanagedSourceDirectories in Test ++= (apronJar.value map { _ => (sourceDirectory in Test).value / "apron" }).toSeq

//*** Assembly plugin

test in assembly := { }

assemblyMergeStrategy in assembly ~= { (oldStrategy) =>  {
    case PathList(ps @ _*) if ps.last.endsWith(".class")  => MergeStrategy.last
    case x => oldStrategy(x)
  }
}

//*** Eclipse plugin

EclipseKeys.configurations += Jmh

//*** IDEA plugin

ideOutputDirectory in Compile := Some(file("extended/target/idea/classes"))

ideOutputDirectory in Test := Some(file("extended/target/idea/test-classes"))

//*** JMH Plugin

enablePlugins(JmhPlugin)

dependencyClasspath in Jmh ++= (exportedProducts in Test).value
