val Scala212 = "2.12.8"

ThisBuild / organization := "org.halcat"
ThisBuild / scalaVersion := Scala212
ThisBuild / crossScalaVersions := Seq(Scala212, "2.13.0")
ThisBuild / version := "0.10.0"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/halcat0x15a/kits-eff"),
    "scm:git:git@github.com:halcat0x15a/kits-eff.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "halcat0x15a",
    name = "Sanshiro Yoshida",
    email = "halcat0x15a@gmail.com",
    url = url("http://halcat.org")
  )
)

ThisBuild / description := "Extensible Effects in Scala2"
ThisBuild / licenses := List("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))
ThisBuild / homepage := Some(url("https://github.com/halcat0x15a/kits-eff"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

ThisBuild / scalacOptions ++= Seq("-deprecation")

lazy val root = (project in file("."))
  .settings(
    name := "kits-eff",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.0.0",
    libraryDependencies += {
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    }
  )
