fork  := true

unmanagedJars in Compile += file("/usr/local/lib/ppl/ppl_java.jar")

javaOptions += "-Djava.library.path=/usr/local/lib/ppl"

