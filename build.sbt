
lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-scala3",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC2",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang.modules" % "scala-parallel-collections_2.13" % "1.0.0"
  )
