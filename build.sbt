import com.typesafe.tools.mima.core._
import com.github.sbt.git.SbtGit.GitKeys.gitHeadCommit

ThisBuild / tlBaseVersion := "1.11"
ThisBuild / tlVersionIntroduced := Map("2.12" -> "1.11.4", "2.13" -> "1.11.4")
ThisBuild / tlMimaPreviousVersions ~= (_.filterNot(Set("1.11.5", "1.11.6")))

ThisBuild / developers := List(
  tlGitHubDev("mpilquist", "Michael Pilquist"),
  tlGitHubDev("pchiusano", "Paul Chiusano")
)

ThisBuild / organization := "org.scodec"
ThisBuild / organizationHomepage := Some(new URL("http://scodec.org"))
ThisBuild / tlSonatypeUseLegacyHost := true
ThisBuild / licenses := List(
  (
    "Three-clause BSD-style",
    url(
      "https://github.com/scodec/scodec/blob/master/LICENSE"
    )
  )
)
ThisBuild / crossScalaVersions := List("2.12.16", "2.13.16")

ThisBuild / tlCiReleaseBranches := List("series/1.11.x")
ThisBuild / tlCiHeaderCheck := false
ThisBuild / tlFatalWarnings := false

lazy val commonSettings = Seq(
  Compile / unmanagedResources ++= {
    val base = baseDirectory.value
    (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get
  },
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
)
lazy val commonJsSettings = Seq(
  tlVersionIntroduced := List("2.12", "2.13").map(_ -> "1.11.5").toMap
)
lazy val commonNativeSettings = Seq(
  tlVersionIntroduced := List("2.12", "2.13").map(_ -> "1.11.10").toMap
)

ThisBuild / libraryDependencySchemes +=
  "org.scala-native" %% "test-interface_native0.5" % VersionScheme.Always

lazy val root = tlCrossRootProject.aggregate(testkit, core, unitTests)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "scodec-core",
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.2.1",
      "com.chuusai" %%% "shapeless" % "2.3.12"
    ),
    buildInfoPackage := "scodec",
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion, gitHeadCommit),
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingMethodProblem]("scodec.codecs.UuidCodec.codec"),
      ProblemFilters.exclude[MissingMethodProblem]("scodec.Attempt.toTry"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.compat.FactoryOps"),
      ProblemFilters.exclude[MissingClassProblem]("scodec.compat$FactoryOps")
    )
  )
  .jsSettings(commonJsSettings)
  .jsSettings(
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingClassProblem]("scodec.codecs.ZlibCodec"),
      ProblemFilters.exclude[Problem]("scodec.codecs.package.zlib*")
    )
  )
  .nativeSettings(commonNativeSettings)

lazy val testkit = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("testkit"))
  .settings(commonSettings: _*)
  .settings(
    name := "scodec-testkit",
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % "1.18.1",
      "org.scalatest" %%% "scalatest" % "3.2.19",
      "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0"
    ),
    tlMimaPreviousVersions ~= (_.filterNot(Set("1.11.4")))
  )
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)
  .dependsOn(core % "compile->compile")

lazy val unitTests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("unitTests"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.70" % "test"
    ),
    libraryDependencies ++= (if (scalaBinaryVersion.value.startsWith("2.10"))
                               Seq(
                                 compilerPlugin(
                                   ("org.scalamacros" % "paradise" % "2.0.1")
                                     .cross(CrossVersion.patch)
                                 )
                               )
                             else Nil)
  )
  .dependsOn(testkit % "test->compile")
  .enablePlugins(NoPublishPlugin)

lazy val benchmark: Project = project
  .in(file("benchmark"))
  .dependsOn(core.jvm)
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(commonSettings: _*)
