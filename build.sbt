name in ThisBuild := "Scala-2.13-Playground"

version in ThisBuild := "0.1"

scalaVersion in ThisBuild := "2.13.1"
scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
)

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.3",
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