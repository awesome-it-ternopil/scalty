val usernameOpt = Option(System.getenv().get("SONATYPE_USERNAME"))
val passwordOpt = Option(System.getenv().get("SONATYPE_PASSWORD"))
val passphrase  = Option(System.getenv().get("PGP_PASSPHRASE")).map(_.toCharArray)

credentials ++= usernameOpt
  .zip(passwordOpt)
  .map {
    case (username, password) =>
      Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
  }
  .toSeq

pgpPassphrase := passphrase
