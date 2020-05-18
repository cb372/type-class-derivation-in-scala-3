val dottyVersion = "0.25.0-bin-20200516-9450bf2-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion
  )
