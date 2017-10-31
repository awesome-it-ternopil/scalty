name := "scalty"

version := "0.0.1"

scalaVersion := "2.11.8"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "org.typelevel"  %% "cats-core"  % "1.0.0-MF"
libraryDependencies += "org.scalactic"  %% "scalactic"  % "3.0.0"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
libraryDependencies += "org.scalatest"  %% "scalatest"  % "3.0.0" % "test"
