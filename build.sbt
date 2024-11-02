

val scala3Version: String = "3.4.2"
organization := "com.peterlavalle"
scalaVersion := scala3Version

name := "Resource Template"

resolvers += "Maven Central" at "https://repo1.maven.org/maven2/"

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
