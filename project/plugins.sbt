logLevel := Level.Warn

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.4.0")

addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.5")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"