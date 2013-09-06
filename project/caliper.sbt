resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
      Resolver.ivyStylePatterns)

addSbtPlugin("me.lessis" % "cappi" % "0.1.1")
