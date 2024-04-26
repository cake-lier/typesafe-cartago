import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

lazy val root = (project in file("."))
  .settings(
    name := "typesafe-cartago",
    idePackagePrefix := Some("io.github.cakelier"),
    libraryDependencies ++= Seq("eu.timepit" %% "refined" % "0.11.1")
  )
