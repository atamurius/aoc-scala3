
lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-scala3",
    version := "1.0.0",

    Compile/mainClass := Some("common.AutoRunner"),

    scalaVersion := "3.0.0",

    libraryDependencies += "org.scala-lang.modules" % "scala-parallel-collections_3" % "1.0.4"
  )
