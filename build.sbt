import com.typesafe.tools.mima.core._
import sbtcrossproject.crossProject

val commonSettings = Seq(
  scodecModule := "scodec-core",
  githubProject := "scodec",
  rootPackage := "scodec",
  scmInfo := Some(ScmInfo(url("https://github.com/scodec/scodec"), "git@github.com:scodec/scodec.git")),
  contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano")),
  crossScalaVersions := "2.10.6" +: crossScalaVersions.value.filterNot(_.startsWith("2.10.")).filterNot(_.startsWith("2.13.")),
  releaseCrossBuild := true
)

lazy val root = project.in(file(".")).aggregate(
  testkitJVM, testkitJS,
  coreJVM, coreJS,
  unitTests).
  settings(commonSettings: _*).settings(
  publishArtifact := false
)

lazy val core = crossProject(JVMPlatform, JSPlatform).in(file(".")).
  enablePlugins(BuildInfoPlugin).
  enablePlugins(ScodecPrimaryModuleSettings).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.8",
      "com.chuusai" %%% "shapeless" % "2.3.3"
    ),
    libraryDependencies ++= (if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.patch)) else Nil)
  ).
  jvmSettings(
    docSourcePath := new File(baseDirectory.value, ".."),
    OsgiKeys.exportPackage := Seq("!scodec.bits,scodec.*;version=${Bundle-Version}"),
    mimaPreviousArtifacts := mimaPreviousArtifacts.value.map(p => p.withName(p.name.replace("core", "scodec-core"))),
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingMethodProblem]("scodec.codecs.UuidCodec.codec"),
      ProblemFilters.exclude[MissingMethodProblem]("scodec.Attempt.toTry")
    )
  ).
  jsSettings(commonJsSettings: _*)

lazy val coreJVM = core.jvm.enablePlugins(ScodecPrimaryModuleJVMSettings)
lazy val coreJS = core.js

lazy val testkit = crossProject(JVMPlatform, JSPlatform).in(file("testkit")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.8",
      "com.chuusai" %%% "shapeless" % "2.3.3",
      "org.scalacheck" %%% "scalacheck" % "1.13.5",
      "org.scalatest" %%% "scalatest" % "3.0.5"
    )
  ).
  jsSettings(commonJsSettings: _*).
  dependsOn(core % "compile->compile")

lazy val testkitJVM = testkit.jvm
lazy val testkitJS = testkit.js

lazy val unitTests = project.in(file("unitTests")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.60" % "test"
    ),
    libraryDependencies ++= (if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.patch)) else Nil)
  ).
  dependsOn(testkitJVM % "test->compile").
  settings(publishArtifact := false)

lazy val benchmark: Project = project.in(file("benchmark")).dependsOn(coreJVM).enablePlugins(JmhPlugin).
  settings(commonSettings: _*).
  settings(publishArtifact := false)
