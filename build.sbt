ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "finance-model",
     libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10",
     libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
  )
