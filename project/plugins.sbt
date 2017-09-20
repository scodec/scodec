resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

addSbtPlugin("org.scodec" % "scodec-build" % "1.7.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native"         % "0.3.6")

addSbtPlugin("org.scala-native" % "sbt-crossproject"         % "0.2.2")
addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.2")

addSbtPlugin("com.dwijnand" % "sbt-dynver" % "2.0.0")
