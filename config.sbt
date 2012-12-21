unmanagedClasspath in Compile += file("/usr/local/lib/ppl/ppl_java.jar")

unmanagedClasspath in Test += file("/usr/local/lib/ppl/ppl_java.jar")

unmanagedClasspath in Runtime += file("/usr/local/lib/ppl/ppl_java.jar")

javaOptions += "-Djava.library.path=/usr/local/lib/ppl"

