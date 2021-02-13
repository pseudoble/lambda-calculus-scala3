val dottyVersion = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
        ("org.scalatest" %% "scalatest" % "3.2.4-M1" % Test),
        ("org.scalatestplus" %% "scalacheck-1-15" % "3.2.4.0-M1" % Test).intransitive().withDottyCompat(scalaVersion.value),
        ("org.scalacheck" %% "scalacheck" % "1.15.2" % Test).withDottyCompat(scalaVersion.value),
        ("org.scalactic" %% "scalactic" % "3.2.4-M1" % Test),
    )
  )
