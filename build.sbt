libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

lazy val commonSettings = Seq(
  scalaVersion := "2.10.1"
)

lazy val root = (project in file("."))
.settings(
  commonSettings,
  name := "Splay Tree"
)
