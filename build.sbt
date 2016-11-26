name := "alacarte"

version := "1.0"

scalaVersion := "2.12.0"

scalacOptions ++= Seq(
  "-unchecked",
  "-feature",
  "-deprecation:false",
  "-Xlint",
  "-Xcheckinit",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code",
  "-Yno-adapted-args",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding", "UTF-8"
)

libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"

tutSettings

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
