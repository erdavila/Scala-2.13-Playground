name := "lzw"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
