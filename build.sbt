name := "rect"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "joda-time" % "joda-time" % "2.8.1",
  "org.apache.commons" % "commons-lang3" % "3.1",
  "org.apache.poi" % "poi" % "3.13",
  "org.apache.poi" % "poi-ooxml" % "3.13",
  "org.apache.poi" % "poi-ooxml-schemas" % "3.13",
  "org.apache.poi" % "ooxml-schemas" % "1.3",
  "junit" % "junit" % "4.12",
  "org.scalatest" % "scalatest_2.12" % "3.0.3"
)