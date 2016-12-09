//*** Additional source directories for PPL

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

//*** Assembly plugin

test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => 
  {
    case PathList(ps @ _*) if ps.last.endsWith(".class")  => MergeStrategy.last
    case x => old(x)
  }
}

//*** Eclipse plugin

EclipseKeys.createSrc := EclipseCreateSrc.ValueSet()

EclipseKeys.configurations += config("jmh")

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

EclipseKeys.eclipseOutput := Some("target.eclipse")

incOptions := incOptions.value.withLogRecompileOnMacro(false)

//*** JMH Plugin

enablePlugins(JmhPlugin)

run in Jmh <<= (run in Jmh) dependsOn (compile in Jmh)

// We prefer to change resourceDirectories instead of resourceDirectory so that directories do not
// appear in the Eclipse project.
resourceDirectories in Jmh := ((baseDirectory in ThisBuild).value / "core" / "src" / "test" / "resources") +:
  ( (managedResourceDirectories in Jmh).value ++ (unmanagedResourceDirectories in Jmh).value )

dependencyClasspath in Jmh := (fullClasspath in Test).value
