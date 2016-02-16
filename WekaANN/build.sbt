organization := "feh.tec"

name := "WekaANN"

scalaVersion := "2.11.7"

val wekaJar = file(sys.env("WEKA_HOME")) / "weka.jar"

unmanagedClasspath in Compile += wekaJar

unmanagedClasspath in Runtime += wekaJar


resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.9-SNAPSHOT"
