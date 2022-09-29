val ScalaVersion = "3.2.0"
val Version      = "0.1.0"

val TestDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
)

lazy val fps = project
  .in(file("fps"))
  .settings( name           := "fps"
    , version              := Version
    , scalaVersion         := ScalaVersion
    , libraryDependencies ++= TestDependencies
  )

lazy val aoc = project
  .in(file("AdventOfCode2018"))
  .settings( name           := "AdventOfCode2018"
    , version              := Version
    , scalaVersion         := ScalaVersion
    , libraryDependencies ++= TestDependencies
  )
