import com.typesafe.tools.mima.core._

ThisBuild / tlBaseVersion := "2.2"

ThisBuild / organization := "org.scodec"
ThisBuild / organizationName := "Scodec"

ThisBuild / startYear := Some(2013)

ThisBuild / crossScalaVersions := Seq("3.2.2")

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8"))

ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/scodec/scodec"), "git@github.com:scodec/scodec.git")
)

ThisBuild / licenses := List(
  ("BSD-3-Clause", url("https://github.com/scodec/scodec/blob/main/LICENSE"))
)

ThisBuild / developers ++= List(
  tlGitHubDev("mpilquist", "Michael Pilquist"),
  tlGitHubDev("pchiusano", "Paul Chiusano")
)

ThisBuild / mimaBinaryIssueFilters ++= Seq(
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.IsoLowPriority.toTuple"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.IsoLowPriority.fromTuple"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$2"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$3"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$4"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$5"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$2"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$3"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$4"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("scodec.codecs.codecs#package.zlib$default$5")
)

lazy val root = tlCrossRootProject.aggregate(testkit, core, unitTests, benchmarks)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "scodec-core",
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.36"
    ),
    scalacOptions := scalacOptions.value
      .filterNot(_ == "-source:3.0-migration") :+ "-source:future",
    Test / scalacOptions := (Compile / scalacOptions).value,
    Compile / unmanagedResources ++= {
      val base = baseDirectory.value
      (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get
    }
  )

lazy val coreJS = core.js.settings(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[MissingClassProblem]("scodec.codecs.ZlibCodec")
  )
)

lazy val coreNative = core.native.settings(
  tlVersionIntroduced ++= List("2.12", "2.13", "3").map(_ -> "2.2.0").toMap
)

lazy val testkit = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .settings(
    name := "scodec-testkit",
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.0.0-M7",
    scalacOptions := scalacOptions.value.filterNot(_ == "-source:3.0-migration") :+ "-source:future"
  )
  .dependsOn(core % "compile->compile")

lazy val testkitJVM = testkit.jvm
lazy val testkitJS = testkit.js
lazy val testkitNative = testkit.native.settings(
  tlVersionIntroduced ++= List("2.12", "2.13", "3").map(_ -> "2.2.0").toMap
)

lazy val unitTests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .settings(
    scalacOptions := scalacOptions.value.filterNot(
      _ == "-source:3.0-migration"
    ) :+ "-source:future",
    Test / scalacOptions := (Compile / scalacOptions).value
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcpkix-jdk18on" % "1.72" % Test
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
        .cross(CrossVersion.for3Use2_13)
    )
  )
  .dependsOn(testkit % "test->compile")
  .enablePlugins(NoPublishPlugin)

lazy val benchmarks = project
  .dependsOn(core.jvm)
  .enablePlugins(JmhPlugin, NoPublishPlugin)
