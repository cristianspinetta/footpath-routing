import Dependencies.{compile => _, test => _, _}
import sbt._
import sbt.Keys._

object Projects extends Build {
  import Dependencies._
  import Settings._

  lazy val root = Project("footpath-routing", file("."))
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(noPublishing: _*)
    .aggregate(pathGenerator, mapGenerator, navigationApi, mapDomain, commonLibrary)

  lazy val pathGenerator = Project("path-generator", file("path-generator"))
    .dependsOn(commonLibrary, mapDomain)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, slf4jApi ,logbackCore, logbackClassic, akkaActor, akkaSlf4j, akkaTestKit,
        scalaReflect) ++
        test(scalatest, mockito))
    .settings(noPublishing: _*)

  lazy val mapGenerator = Project("map-generator", file("map-generator"))
    .dependsOn(mapDomain, pathGenerator, commonLibrary)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, slf4jApi ,logbackCore, logbackClassic, akkaActor, akkaSlf4j, akkaTestKit,
        jodaTime, scalaReflect, scalaXml, json4sJackson) ++
        test(scalatest, mockito, scalacheck))
    .settings(noPublishing: _*)

  lazy val navigationApi = Project("navigation-api", file("navigation-api"))
    .dependsOn(pathGenerator, mapGenerator, mapDomain, commonLibrary)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, slf4jApi, logbackCore, logbackClassic, akkaActor, akkaStream, akkaHttpExperimental,
        akkaHttpSprayJsonExperimental, akkaHttpTestKit, akkaHttpCors, scalikejdbc, scalikejdbcConfig, mariadbConnector, commonsPool, commonsDbcp) ++
        test(scalatest, mockito))
    .settings(noPublishing: _*)
    .settings(settingsForPlayground: _*)
    .settings(mainClass in (Compile, run) := Some("api.WebServer"))

  lazy val mapDomain = Project("map-domain", file("map-domain"))
    .dependsOn(commonLibrary)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, logbackCore, logbackClassic, scalikejdbc, scalikejdbcConfig, mariadbConnector, commonsPool, commonsDbcp, h2Connector, h2gis) ++
        test(scalatest, mockito, scalacheck))
    .settings(noPublishing: _*)

  lazy val commonLibrary = Project("common-library", file("common-library"))
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, logbackCore, logbackClassic) ++
        test(scalatest, mockito))
    .settings(noPublishing: _*)

  val noPublishing = Seq(publish := (), publishLocal := (), publishArtifact := false)
}
