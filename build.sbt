name := "WTask"

version := "1.0.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")