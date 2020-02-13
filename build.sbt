ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "org.rise-lang"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-Xlint",
    "-Xmax-classfile-name", "100",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls"
  ),
  fork := true,
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val shine = (project in file("."))
  .aggregate(executor)
  .dependsOn(shineMacros, executor, elevate, rise)
  .settings(
    name    := "shine",
    version := "1.0",

    commonSettings,

    javaOptions ++= Seq("-Djava.library.path=lib/executor/lib/Executor/build", "-Xss20m"),

    // Scala libraries
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value,
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0",

    // JUnit
    libraryDependencies += "junit" % "junit" % "4.11",

    // Scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test",

    // Silencer: Scala compiler plugin for warning suppression
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.3" cross CrossVersion.full),
      "com.github.ghik" %% "silencer-lib" % "1.4.3" % Provided cross CrossVersion.full
    )
  )

lazy val executor   = (project in file("lib/executor"))

lazy val rise       = (project in file("lib/rise"))

lazy val elevate    = (project in file("lib/elevate"))

lazy val shineMacros = (project in file("macros"))
  .settings(
    name := "macros",
    version := "1.0",
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  )
