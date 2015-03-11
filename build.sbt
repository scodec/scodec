import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaKeys._

scodecModule := "scodec-core"

scodecPrimaryModule

contributors ++= Seq(Contributor("mpilquist", "Michael Pilquist"), Contributor("pchiusano", "Paul Chiusano"))

rootPackage := "scodec"

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-bits" % "1.0.6-SNAPSHOT",
  "com.chuusai" % "shapeless" % "2.1.0" cross CrossVersion.fullMapped {
    case "2.10.4" => "2.10.4"
    case x if x startsWith "2.11." => "2.11"
  },
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.50" % "test"
)

// Shapeless 2.1.0 on Scala 2.10 requires macro paradise
libraryDependencies ++= {
  if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)) else Nil
}

OsgiKeys.exportPackage := Seq("!scodec.bits,scodec.*;version=${Bundle-Version}")

binaryIssueFilters ++= Seq(
)
