import AssemblyKeys._

unmanagedSources in Compile := (unmanagedSources in Compile).value filter {
  source => pplJar.value.isDefined || ! source.getParent.endsWith("ppl")
}

unmanagedSources in Test := (unmanagedSources in Test).value filter {
  source => pplJar.value.isDefined || ! source.getParent.endsWith("ppl")
}

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

