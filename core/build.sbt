// PPL options which cannot be specified at the build level (why?)

unmanagedJars in Compile += file("/usr/local/lib/ppl/ppl_java.jar")

EclipseKeys.classpathTransformerFactories += NativeLibTransformerFactory("/usr/local/lib/ppl/ppl_java.jar")

// eclipse-sbt plugin configuration cannot be specified at the build level
EclipseKeys.configurations := Set(Compile, Test, Benchmark)

// add resources to classpath. We need a plugin which allows to
// exclude resource directories from compilation.
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource