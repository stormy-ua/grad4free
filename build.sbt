import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.apache.spark" %% "spark-mllib" % "2.2.0",
      "org.scalaz" %% "scalaz-core" % "7.2.15",
      "org.typelevel" %% "cats-core" % "1.0.0-MF",
      "org.typelevel" %% "cats-free" % "1.0.0-MF",
      "com.chuusai" %% "shapeless" % "2.3.2"
    )
  )
