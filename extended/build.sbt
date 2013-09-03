import AssemblyKeys._

// PPL options which cannot be specified at the build level (why?)

unmanagedJars in Compile += file("/usr/local/lib/ppl/ppl_java.jar")

EclipseKeys.classpathTransformerFactories += NativeLibTransformerFactory("/usr/local/lib/ppl/ppl_java.jar")

// assembly plugin configuration

assemblySettings

test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => 
  {
    case PathList(ps @ _*) if ps.last.endsWith(".class")  => MergeStrategy.last
    case x => old(x)
  }
}

// cappi benchmarks

seq(cappiSettings:_*)
