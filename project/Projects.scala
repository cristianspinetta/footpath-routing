import Dependencies.{compile => _, test => _, _}
import sbt._
import sbt.Keys._

object Projects extends Build {
  import Dependencies._
  import Settings._
  import sbtassembly.AssemblyKeys._

  lazy val root = Project("footpath-routing", file("."))
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(noPublishing: _*)
    .aggregate(pathGenerator, mapGenerator, navigationApi, mapDomain, commonLibrary)
    .settings(notAggregateInAssembly: _*)

  lazy val pathGenerator = Project("path-generator", file("path-generator"))
    .dependsOn(commonLibrary, mapDomain)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, slf4jApi ,logbackCore, logbackClassic, akkaActor, akkaSlf4j,
        scalaReflect) ++
        test(scalatest, mockito, akkaTestKit))
    .settings(noPublishing: _*)
    .settings(notAggregateInAssembly: _*)

  lazy val mapGenerator = Project("map-generator", file("map-generator"))
    .dependsOn(mapDomain, pathGenerator, commonLibrary)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, slf4jApi ,logbackCore, logbackClassic, akkaActor, akkaSlf4j,
        jodaTime, scalaReflect, scalaXml, json4sJackson) ++
        test(scalatest, mockito, scalacheck, akkaTestKit))
    .settings(noPublishing: _*)
    .settings(notAggregateInAssembly: _*)

  lazy val navigationApi = Project("navigation-api", file("navigation-api"))
    .dependsOn(pathGenerator, mapGenerator, mapDomain, commonLibrary)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(assemblySettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, slf4jApi, logbackCore, logbackClassic, akkaActor, akkaStream, akkaHttpExperimental, akkaHttpSprayJsonExperimental,
        akkaHttpCors, scalikejdbc, scalikejdbcConfig, mariadbConnector, commonsPool, commonsDbcp, cats) ++
        test(scalatest, mockito, akkaHttpTestKit))
    .settings(noPublishing: _*)
    .settings(playgroundSettings: _*)
    .settings(mainClass in (Compile, run) := Some("api.WebServer"))
    .settings(mainClass in assembly := Some("api.WebServer"))

  lazy val mapDomain = Project("map-domain", file("map-domain"))
    .dependsOn(commonLibrary, snapshots)
    .configs(BenchmarkConfig, DBTestsConfig)
    .settings(testsSettings: _*)
    .settings(benchmarkSettings: _*)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
        compile(typesafeConfig, logbackCore, logbackClassic, scalikejdbc, scalikejdbcConfig, mariadbConnector, commonsPool, commonsDbcp) ++
        test(scalatest, mockito, scalacheck, scalaMeter))
    .settings(noPublishing: _*)
    .settings(notAggregateInAssembly: _*)

  lazy val commonLibrary = Project("common-library", file("common-library"))
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, logbackCore, logbackClassic, json4sJackson, jacksonScala) ++
        test(scalatest, mockito))
    .settings(noPublishing: _*)
    .settings(notAggregateInAssembly: _*)

  lazy val snapshots = Project("snapshots", file("snapshots"))
    .dependsOn(commonLibrary)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, logbackCore, logbackClassic) ++
        test(scalatest, mockito))
    .settings(noPublishing: _*)
    .settings(notAggregateInAssembly: _*)

  lazy val routingPlayground = Project("routing-playground", file("routing-playground"))
    .dependsOn(pathGenerator, mapGenerator, mapDomain, commonLibrary)
    .settings(basicSettings: _*)
    .settings(formatSettings: _*)
    .settings(libraryDependencies ++=
      compile(typesafeConfig, slf4jApi, logbackCore, logbackClassic, akkaActor, akkaStream, akkaHttpExperimental,
        akkaHttpSprayJsonExperimental, akkaHttpCors, scalikejdbc, scalikejdbcConfig, mariadbConnector, commonsPool, commonsDbcp) ++
        test(scalatest, mockito, akkaHttpTestKit))
    .settings(noPublishing: _*)
    .settings(notAggregateInAssembly: _*)
    .settings(playgroundSettings: _*)
    .settings(mainClass in (Compile, run) := Some("api.WebServer"))

  val noPublishing = Seq(publish := (), publishLocal := (), publishArtifact := false)
}
