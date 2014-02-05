import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._

organization := "com.github.scodec"

name := "scodec"

scalaVersion := "2.10.3"

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

licenses += ("Three-clause BSD-style", url("http://github.com/mpilquist/scodec/blob/master/LICENSE"))

unmanagedResources in Compile <++= baseDirectory map { base => (base / "NOTICE") +: (base / "LICENSE") +: ((base / "licenses") * "LICENSE_*").get }

triggeredMessage := (_ => Watched.clearScreen)

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalaz" %% "scalaz-core" % "7.0.4",
  "com.chuusai" %% "shapeless" % "1.2.4",
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.49" % "test",
  "com.google.guava" % "guava" % "14.0.1" % "test"
)

osgiSettings

OsgiKeys.exportPackage := Seq("scodec.*;version=${Bundle-Version}")

OsgiKeys.importPackage := Seq(
  """scala.*;version="$<range;[==,=+);$<@>>"""",
  """scalaz.*;version="$<range;[==,=+);$<@>>"""",
  "*"
)

OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>http://github.com/scodec/scodec</url>
  <scm>
    <url>git@github.com:scodec/scodec.git</url>
    <connection>scm:git:git@github.com:scodec/scodec.git</connection>
  </scm>
  <developers>
    <developer>
      <id>mpilquist</id>
      <name>Michael Pilquist</name>
      <url>http://github.com/mpilquist</url>
    </developer>
    <developer>
      <id>pchiusano</id>
      <name>Paul Chiusano</name>
      <url>http://github.com/pchiusano</url>
    </developer>
  </developers>
)

pomPostProcess := { (node) =>
  import scala.xml._
  import scala.xml.transform._
  def stripIf(f: Node => Boolean) = new RewriteRule {
    override def transform(n: Node) =
      if (f(n)) NodeSeq.Empty else n
  }
  val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
  new RuleTransformer(stripTestScope).transform(node)(0)
}

releaseSettings

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts.copy(action = publishSignedAction),
  setNextVersion,
  commitNextVersion,
  pushChanges
)
