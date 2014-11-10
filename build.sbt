name := "scalop"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "com.storm-enroute" %% "scalameter" % "0.6" % "test"
)
