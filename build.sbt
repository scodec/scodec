import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaKeys._

val commonSettings = Seq(
  scodecModule := "scodec-core",
  rootPackage := "scodec",
  contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano"))
)

lazy val root = project.in(file(".")).aggregate(coreJVM, coreJS).settings(commonSettings: _*).settings(
  publish := {},
  publishLocal := {}
)

lazy val core = crossProject.in(file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(scodecPrimaryModule: _*).
  jvmSettings(scodecPrimaryModuleJvm: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.0.7",
      "com.chuusai" %%% "shapeless" % "2.2.2",
      "org.scalatest" %%% "scalatest" % "3.0.0-M2" % "test",
      "org.scalacheck" %%% "scalacheck" % "1.12.3" % "test"
    ),
    libraryDependencies ++= (if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)) else Nil)
  ).
  jvmSettings(
    OsgiKeys.exportPackage := Seq("!scodec.bits,scodec.*;version=${Bundle-Version}"),
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.50" % "test"
    ),
    binaryIssueFilters ++= Seq(
    )
  ).
  jsSettings(commonJsSettings: _*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
