import AssemblyKeys._

unmanagedSourceDirectories in Compile ++= (pplJar.value map { _ => (sourceDirectory in Compile).value / "ppl" }).toSeq

unmanagedSourceDirectories in Test ++= (pplJar.value map { _ => (sourceDirectory in Test).value / "ppl" }).toSeq

// Assembly plugin configuration

assemblySettings

test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => 
  {
    case PathList(ps @ _*) if ps.last.endsWith(".class")  => MergeStrategy.last
    case x => old(x)
  }
}

// Cappi plugin configurations

cappiSettings

