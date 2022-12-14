val ScalaVersion = "3.2.0"
val Version      = "0.1.0"

val TestDependencies = Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.14" % "test"
)

lazy val fps = project
  .in(file("fps"))
  .settings( name           := "fps"
    , version              := Version
    , scalaVersion         := ScalaVersion
    , libraryDependencies ++= TestDependencies
  )

lazy val aoc = project
  .in(file("aoc"))
  .settings( name           := "aoc"
    , version              := Version
    , scalaVersion         := ScalaVersion
    , libraryDependencies ++= TestDependencies
  )
