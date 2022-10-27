
lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-scala3",
    version := "1.0.0",

    scalaVersion := "3.2.0",

    libraryDependencies += "org.scala-lang.modules" % "scala-parallel-collections_2.13" % "1.0.0"
  )
