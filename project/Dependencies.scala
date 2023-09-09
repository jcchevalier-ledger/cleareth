import sbt.*
object Dependencies {
  lazy val `cats-core`      = "org.typelevel"   %% "cats-core"      % "2.10.0"
  lazy val `bcprov-jdk18on` = "org.bouncycastle" % "bcprov-jdk18on" % "1.76"
  lazy val `scodec-core`    = "org.scodec"      %% "scodec-core"    % "2.2.1"

  lazy val `scalaTest`           = "org.scalatest"     %% "scalatest"           % "3.2.17"
  lazy val `munit-cats-effect-3` = "org.typelevel"     %% "munit-cats-effect-3" % "1.0.7"
  lazy val `munit`               = "org.scalameta"     %% "munit"               % "0.7.29"
  lazy val `scalacheck-1-17`     = "org.scalatestplus" %% "scalacheck-1-17"     % "3.2.16.0"
}
