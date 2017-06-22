name := "Xsystem"

version := "1.0"

resolvers ++= Seq(
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
    "justwrote" at "http://repo.justwrote.it/releases"

)

libraryDependencies ++= Seq(
    "org.scala-saddle" %% "saddle-core" % "1.3.+"
)

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"
libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"
libraryDependencies += "it.justwrote" %% "scala-faker" % "0.3"

scalaVersion := "2.11.1"