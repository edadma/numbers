ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

lazy val numbers = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file(".")).
  settings(
    name := "numbers",
    version := "0.1.2",
    scalaVersion := "2.13.6",
    scalacOptions ++=
      Seq(
        "-deprecation", "-feature", "-unchecked",
        "-language:postfixOps", "-language:implicitConversions", "-language:existentials", "-language:dynamics",
        "-Xasync"
      ),
    organization := "xyz.hyperreal",
    githubOwner := "edadma",
    githubRepository := "numbers",
    mainClass := Some("xyz.hyperreal.numbers.Main"),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.5" % "test",
    publishMavenStyle := true,
    Test / publishArtifact := false
  ).
  jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided",
  ).
  nativeSettings(
    nativeLinkStubs := true
  ).
  jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
  )
