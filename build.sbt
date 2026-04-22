val scala3Version = "3.3.5"

lazy val root = project
  .in(file("."))
  .settings(
      name := "pps-lab06",
      version := "0.1.0-SNAPSHOT",
      scalaVersion := scala3Version,
    libraryDependencies += "net.aichler" % "jupiter-interface" % "0.9.0" % Test
  )
