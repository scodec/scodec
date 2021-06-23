import com.typesafe.tools.mima.core._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import com.typesafe.sbt.SbtGit.GitKeys.{gitCurrentBranch, gitHeadCommit}

addCommandAlias("fmt", "; compile:scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", "; compile:scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

ThisBuild / baseVersion := "2.0"

ThisBuild / organization := "org.scodec"
ThisBuild / organizationName := "Scodec"

ThisBuild / homepage := Some(url("https://github.com/scodec/scodec"))
ThisBuild / startYear := Some(2013)

ThisBuild / crossScalaVersions := Seq("3.0.0")

ThisBuild / strictSemVer := false

ThisBuild / versionIntroduced := Map.empty

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")

ThisBuild / spiewakCiReleaseSnapshots := true

ThisBuild / spiewakMainBranches := List("main")

ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/scodec/scodec"), "git@github.com:scodec/scodec.git")
)

ThisBuild / licenses := List(
  ("BSD-3-Clause", url("https://github.com/scodec/scodec/blob/main/LICENSE"))
)

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

ThisBuild / publishGithubUser := "mpilquist"
ThisBuild / publishFullName := "Michael Pilquist"
ThisBuild / developers ++= List(
  "pchiusano" -> "Paul Chiusano"
).map { case (username, fullName) =>
  Developer(username, fullName, s"@$username", url(s"https://github.com/$username"))
}

ThisBuild / fatalWarningsInCI := false

ThisBuild / mimaBinaryIssueFilters ++= Seq(
)

lazy val root = project
  .in(file("."))
  .aggregate(testkitJVM, testkitJS, coreJVM, coreJS, unitTests, benchmarks)
  .enablePlugins(NoPublishPlugin, SonatypeCiReleasePlugin)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("."))
  .settings(
    name := "scodec-core",
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.27"
    ),
    scalacOptions := scalacOptions.value.filterNot(_ == "-source:3.0-migration") :+ "-source:future",
    Test / scalacOptions := (Compile / scalacOptions).value,
    Compile / unmanagedResources ++= {
      val base = baseDirectory.value
      (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get
    }
  )
  .jvmSettings(
    OsgiKeys.exportPackage := Seq("!scodec.bits,scodec.*;version=${Bundle-Version}"),
    mimaPreviousArtifacts := {
      List().map { pv =>
        organization.value % (normalizedName.value + "_" + scalaBinaryVersion.value) % pv
      }.toSet
    },
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingMethodProblem]("scodec.codecs.UuidCodec.codec"),
      ProblemFilters.exclude[MissingMethodProblem]("scodec.Attempt.toTry")
    )
  )

lazy val coreJVM = core.jvm.enablePlugins(SbtOsgi).settings(osgiSettings)

lazy val coreJS = core.js.settings(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

lazy val testkit = crossProject(JVMPlatform, JSPlatform)
  .settings(
    name := "scodec-testkit",
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "0.7.26",
    scalacOptions := scalacOptions.value.filterNot(_ == "-source:3.0-migration") :+ "-source:future"
  )
  .dependsOn(core % "compile->compile")

lazy val testkitJVM = testkit.jvm
lazy val testkitJS = testkit.js

lazy val unitTests = project
  .settings(
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.69" % "test"
    ),
    scalacOptions := scalacOptions.value.filterNot(_ == "-source:3.0-migration") :+ "-source:future",
    Test / scalacOptions := (Compile / scalacOptions).value,
  )
  .dependsOn(testkitJVM % "test->compile")
  .enablePlugins(NoPublishPlugin)

lazy val benchmarks = project
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin, NoPublishPlugin)

