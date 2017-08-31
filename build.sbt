import sbt.Resolver

name := "Cheetah"

version := "0.0.1"

scalaVersion := "2.12.3"


resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

lazy val macroAnnotationSettings = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise")) // macroparadise plugin doesn't work in repl yet.
)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "org.bytedeco.javacpp-presets" % "cuda-platform" % "8.0-6.0-1.3",
  "org.bytedeco.javacpp-presets" % "openblas-platform" % "0.2.19-1.3",
  "org.bytedeco.javacpp-presets" % "fftw-platform" % "3.3.5-1.3",
  "io.frees" %% "freestyle" % "0.3.1",
  "io.frees" %% "freestyle-fetch" % "0.3.1",
  "io.frees" %% "freestyle-effects" % "0.3.1",
  "org.typelevel" %% "spire" % "0.14.1"
)