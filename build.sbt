import Dependencies.*

name                         := "cleareth"
ThisBuild / githubOwner      := "LedgerHQ"
ThisBuild / githubRepository := "cleareth"
ThisBuild / versionScheme    := Some("semver-spec")
ThisBuild / organization     := "co.ledger"
ThisBuild / scalaVersion     := "3.3.0"

addCommandAlias("compileall", ";clean;compile;test:compile;scalafmtAll")

ThisBuild / scalacOptions ++= Seq(
  "-Werror",
  "-Wunused:all",
  "-deprecation"
)

lazy val dependencies = Seq(
  libraryDependencies ++= Seq(
    `bcprov-jdk18on`,
    `cats-core`,
    `iron`,
    `scodec-core`
  ),
  libraryDependencies ++= Seq(
    `munit`,
    `munit-cats-effect-3`,
    `scalacheck-1-17`,
    `scalaTest`
  ).map(_ % Test)
)

lazy val `evm-model` =
  project
    .in(file("model"))
    .settings(name := "evm-model")
    .settings(dependencies)
    .enablePlugins(JavaServerAppPackaging)

lazy val `evm-codec` =
  project
    .in(file("codec"))
    .settings(name := "evm-codec")
    .settings(dependencies)
    .dependsOn(`evm-model`)
    .enablePlugins(JavaServerAppPackaging)
