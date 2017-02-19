name := "Xsystem"

version := "1.0"

resolvers ++= Seq(
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
    "org.scala-saddle" %% "saddle-core" % "1.3.+"
)

scalaVersion := "2.11.1"