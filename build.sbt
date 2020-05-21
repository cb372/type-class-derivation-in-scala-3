val dottyVersion = "0.25.0-bin-20200516-9450bf2-NIGHTLY"
val grpcVersion = "1.29.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyVersion
  )
  .aggregate(mu, muService)

// represents the Mu library
lazy val mu = project
  .in(file("mu"))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "io.grpc" % "grpc-netty" % grpcVersion
    )
  )

// represents an application that makes use of the Mu library
lazy val muService = project
  .in(file("mu-service"))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "io.grpc" % "grpc-netty" % grpcVersion
    ),
    fork := true
  )
  .dependsOn(mu)
