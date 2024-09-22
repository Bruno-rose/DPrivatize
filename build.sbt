ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.10"

lazy val root = (project in file("."))
  .settings(
    name := "DPrivatize"
  )

libraryDependencies += "org.apache.commons" % "commons-csv" % "1.10.0"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.4.1"
libraryDependencies += "org.knowm.xchart" % "xchart" % "3.8.3"
libraryDependencies += "org.scalanlp" %% "breeze" % "2.1.0"
libraryDependencies += "com.github.mrpowers" %% "spark-daria" % "1.2.3"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
