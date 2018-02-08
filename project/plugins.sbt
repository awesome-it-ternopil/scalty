logLevel := Level.Warn

resolvers ++= Seq(
  "Artima Maven Repository" at "http://repo.artima.com/releases",
  "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases" at "http://oss.sonatype.org/content/repositories/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)


addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")