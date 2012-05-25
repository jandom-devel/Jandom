name := "Jandom"

version := "0.0.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.9" % "test",
  //"org.jscience" % "jscience" % "4.3.1"
  "org.scalala" %% "scalala" % "1.0.0.RC3-SNAPSHOT"
)

//resolvers += "JCurl repository" at "http://jcurl.berlios.de/m2/repo"