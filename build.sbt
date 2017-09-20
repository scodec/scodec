import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaKeys._

import sbtcrossproject.{crossProject, CrossType}

val commonSettings = Seq(
  scodecModule := "scodec-core",
  githubProject := "scodec",
  rootPackage := "scodec",
  contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano"))
)

lazy val root = project.in(file(".")).aggregate(
  testkitJVM, testkitJS, testkitNative,
  coreJVM, coreJS, coreNative,
  unitTestsJVM, unitTestsJS, unitTestsNative).
  settings(commonSettings: _*).settings(
  publishArtifact := false
)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(scodecPrimaryModule: _*).
  jvmSettings(scodecPrimaryModuleJvm: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.5",
      "com.chuusai" %%% "shapeless" % "2.3.3"
    ),
    libraryDependencies ++= (if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.patch)) else Nil)
  ).
  jvmSettings(
    docSourcePath := new File(baseDirectory.value, ".."),
    OsgiKeys.exportPackage := Seq("!scodec.bits,scodec.*;version=${Bundle-Version}"),
    libraryDependencies ++= Seq(

    ),
    binaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingMethodProblem]("scodec.codecs.UuidCodec.codec"),
      ProblemFilters.exclude[MissingMethodProblem]("scodec.Attempt.toTry")
    )
  ).
  jsSettings(commonJsSettings: _*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

val jvmJsTestkitDeps = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.0",
    "org.scalacheck" %%% "scalacheck" % "1.13.4"
  )
)

val nativeTestkitDeps = Seq(
  libraryDependencies ++= Seq(
    // scalatest and scalactic published locally from master
    "org.scalatest" %%% "scalatest" % "3.0.0-489",
    "org.scalacheck" %%% "scalacheck" % "1.14.0-18db189-SNAPSHOT"
  )
)


lazy val testkit = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("testkit")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.5",
      "com.chuusai" %%% "shapeless" % "2.3.3"
    )
  ).
  jsSettings(commonJsSettings: _*).
  jvmSettings(jvmJsTestkitDeps: _*).
  jsSettings(jvmJsTestkitDeps: _*).
  nativeSettings(nativeTestkitDeps: _*).
  nativeSettings(
    nativeLinkStubs := true
  ).
  dependsOn(core % "compile->compile")

lazy val testkitJVM = testkit.jvm
lazy val testkitJS = testkit.js
lazy val testkitNative = testkit.native

lazy val unitTests = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("unitTests")).
  settings(commonSettings: _*).
  jvmSettings(
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.50" % "test"
    )
  ).
  nativeSettings(
    nativeLinkStubs := true
  ).
  dependsOn(testkit % "test->compile").
  settings(publishArtifact := false)

lazy val unitTestsJVM = unitTests.jvm
lazy val unitTestsJS = unitTests.js
lazy val unitTestsNative = unitTests.native

lazy val benchmark: Project = project.in(file("benchmark")).dependsOn(coreJVM).enablePlugins(JmhPlugin).
  settings(commonSettings: _*).
  settings(publishArtifact := false)
