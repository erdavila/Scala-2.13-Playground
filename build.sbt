name in ThisBuild := "Scala-2.13-Playground"

version in ThisBuild := "0.1"

scalaVersion in ThisBuild := "2.13.2"
scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
)

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  )
)

lazy val macros = (project in file("macros"))
  .settings(commonSettings)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  )

lazy val main = (project in file("main"))
  .dependsOn(macros)
  .settings(commonSettings)
