import sbt._
import sbt.Keys._

object ImplementingFunctionalLanguagesBuild extends Build {

  lazy val typedLambda = Project(
    id = "implementing-functional-languages",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Implementing Functional Languages",
      organization := "net.homeunix.akimichi",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      // libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.1",
	  // libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.2.0"
	  libraryDependencies ++=
      Seq (
		"com.typesafe.akka" % "akka-actor" % "2.0.1",
        "junit" % "junit" % "4.10" % "test",
        "org.scalatest" %% "scalatest" % "1.7.1" % "test",
        "org.scalacheck" %% "scalacheck" % "1.9" % "test",
		"com.googlecode.kiama" %% "kiama" % "1.2.0"
      )
    )
  )
}
