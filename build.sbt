name := "scalop"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

ScoverageSbtPlugin.instrumentSettings

CoverallsPlugin.singleProject

