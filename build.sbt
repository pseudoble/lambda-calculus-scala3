val dottyVersion = "3.0.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      ("org.scalatest" %% "scalatest" % "3.2.5" % Test),
      ("org.scalatestplus" %% "scalacheck-1-15" % "3.2.5.0" % Test).intransitive().withDottyCompat(scalaVersion.value),
      ("org.scalacheck" %% "scalacheck" % "1.15.3" % Test).withDottyCompat(scalaVersion.value),
      ("org.scalactic" %% "scalactic" % "3.2.5" % Test),
    )
  )
