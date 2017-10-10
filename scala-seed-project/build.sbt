import Dependencies._

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version := "0.1.0-SNAPSHOT"
    )),
  name := "Hello",
  resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(scalaTest % Test,
                              "org.scala-lang" % "scala-reflect" % scalaVersion.value,
                              "io.dylemma" %% "xml-spac" % "0.4",
                              "io.monix" %% "monix" % "3.0.0-M1",
                              "org.typelevel" %% "cats-core" % "1.0.0-MF",
                              "com.chuusai" %% "shapeless" % "2.3.2"),
  scalacOptions += "-Ypartial-unification"
)
