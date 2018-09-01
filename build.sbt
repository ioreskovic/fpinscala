lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.14.0"

val commonSettings = Seq(
  scalaVersion := "2.12.6",
  libraryDependencies += scalacheck % Test
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
