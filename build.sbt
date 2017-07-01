name := "Xsystem"

version := "1.0"

resolvers ++= Seq(
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
    "Fabricator" at "http://dl.bintray.com/biercoff/Fabricator"

)

libraryDependencies ++= Seq(
    "org.scala-saddle" %% "saddle-core" % "1.3.+"

)

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"
libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"
libraryDependencies += "com.github.azakordonets" % "fabricator_2.11" % "2.1.4"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4"

scalaVersion := "2.11.1"