name := "petri"

version := "1.0"

mainClass := Some("esadykov.Main")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.5" % "test"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

org.scalastyle.sbt.ScalastylePlugin.Settings

com.github.retronym.SbtOneJar.oneJarSettings

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"