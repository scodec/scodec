import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaKeys._

val commonSettings = Seq(
  scodecModule := "scodec-core",
  githubProject := "scodec",
  rootPackage := "scodec",
  contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano")),
  scalaVersion := "2.12.0-M2",
  crossScalaVersions := Seq(scalaVersion.value),
  scalacOptions in Compile := (scalacOptions in Compile).value.filter {
    case "-Yinline" => false
    case "-Yclosure-elim" => false
    case other => true
  }
)

lazy val root = project.in(file(".")).aggregate(core).settings(commonSettings: _*).settings(
  publishArtifact := false
)

lazy val core = project.in(file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(scodecPrimaryModule: _*).
  settings(scodecPrimaryModuleJvm: _*).
  settings(
    unmanagedSourceDirectories in Compile += baseDirectory.value / "shared" / "src" / "main" / "scala",
    unmanagedSourceDirectories in Test += baseDirectory.value / "shared" / "src" / "test" / "scala",
    unmanagedSourceDirectories in Compile += baseDirectory.value / "jvm" / "src" / "main" / "scala",
    unmanagedSourceDirectories in Test += baseDirectory.value / "jvm" / "src" / "test" / "scala"
  ).
  settings(
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.0.10",
      "com.chuusai" %%% "shapeless" % "2.2.5",
      "org.scalatest" %%% "scalatest" % "2.2.5-M2" % "test",
      "org.scalacheck" %%% "scalacheck" % "1.12.4" % "test"
    ),
    libraryDependencies ++= (if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)) else Nil)
  ).
  settings(
    docSourcePath := new File(baseDirectory.value, ".."),
    OsgiKeys.exportPackage := Seq("!scodec.bits,scodec.*;version=${Bundle-Version}"),
    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcpkix-jdk15on" % "1.50" % "test"
    ),
    binaryIssueFilters ++= Seq(
      ProblemFilters.exclude[MissingMethodProblem]("scodec.codecs.UuidCodec.codec")
    )
  )

lazy val benchmark: Project = project.in(file("benchmark")).dependsOn(core).enablePlugins(JmhPlugin).
  settings(commonSettings: _*).
  settings(publishArtifact := false)
