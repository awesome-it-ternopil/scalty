lazy val root = project
  .in(file("."))
  .settings(
    name := "scalty",
    scalaVersion := "2.12.4",
    crossScalaVersions := Seq("2.12.4", "2.11.12"),
    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-core"  % "1.0.1",
      "org.scalactic"  %% "scalactic"  % "3.0.0",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "org.scalatest"  %% "scalatest"  % "3.0.0" % "test"
    ),
    organization := "com.github.awesome-it-ternopil",
    scalacOptions ++= Seq("-feature", "-deprecation", "-language:implicitConversions"),
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    publishMavenStyle := true,
    licenses += ("MIT license", url("http://opensource.org/licenses/MIT")),
    pomExtra := (
      <url>https://github.com/awesome-it-ternopil/scalty</url>
      <scm>
        <url>https://github.com/awesome-it-ternopil/scalty</url>
        <connection>scm:git@github.com:awesome-it-ternopil/scalty.git</connection>
        <developerConnection>scm:git@github.com:awesome-it-ternopil/scalty.git</developerConnection>
      </scm>
      <developers>
        <developer>
          <id>nkdev91</id>
          <name>Nazar Kisil</name>
          <email>wmailnk@gmail.com</email>
        </developer>
      </developers>
    )
  )
