organization := "com.github.mpilquist"

name := "scodec"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.1"

scalacOptions ++= Seq(
  "-feature",
  "-language:_",
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-Xcheckinit",
  "-Xlint",
  "-Xverify",
  "-Yclosure-elim",
  "-Yinline",
  "-Ywarn-all")

scalacOptions in (Compile, doc) += "-groups"

licenses += ("Three-clause BSD-style", url("http://github.com/cjmx/cjmx/blob/master/LICENSE"))

unmanagedResources in Compile <++= baseDirectory map { base => (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get }

triggeredMessage := (_ => Watched.clearScreen)

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0",
  "com.chuusai" %% "shapeless" % "1.2.4",
  "org.scalatest" %% "scalatest" % "2.0.M6-SNAP6" % "test"
)
