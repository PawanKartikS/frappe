lazy val root = project
  .in(file("."))
  .settings(
    name         := "frappe",
    scalaVersion := "2.13.8"
  )

libraryDependencies += "org.apache.bcel" % "bcel" % "6.5.0"
