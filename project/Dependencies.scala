import sbt._

object Dependencies extends Version {

  val resolutionRepos = Seq(
    Classpaths.typesafeSnapshots,
    "Typesafe Maven Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Typesafe Maven Releases" at "http://repo.typesafe.com/typesafe/releases/"
  )

  val slf4jApi                       = "org.slf4j"                  %  "slf4j-api"                          % "1.7.13"
  val logbackCore                    = "ch.qos.logback"             %  "logback-core"                       % "1.1.3"
  val logbackClassic                 = "ch.qos.logback"             %  "logback-classic"                    % "1.1.3"
  val typesafeConfig                 = "com.typesafe"               %  "config"                             % "1.3.0"
  val scalatest                      = "org.scalatest"             %%  "scalatest"                          % "3.0.0"
  val scalacheck                     = "org.scalacheck"            %%  "scalacheck"                         % "1.13.2"
  val mockito                        = "org.mockito"                %  "mockito-core"                       % "2.0.42-beta"
  val akkaActor                      = "com.typesafe.akka"         %%  "akka-actor"                         % AkkaVersion
  val akkaStream                     = "com.typesafe.akka"         %%  "akka-stream"                        % AkkaVersion
  val akkaHttpExperimental           = "com.typesafe.akka"         %%  "akka-http-experimental"             % AkkaVersion
  val akkaHttpSprayJsonExperimental  = "com.typesafe.akka"         %%  "akka-http-spray-json-experimental"  % AkkaVersion
  val akkaHttpTestKit                = "com.typesafe.akka"         %%  "akka-http-testkit"                  % AkkaVersion
  val akkaSlf4j                      = "com.typesafe.akka"         %%  "akka-slf4j"                         % AkkaVersion
  val akkaTestKit                    = "com.typesafe.akka"         %%  "akka-testkit"                       % AkkaVersion
  val akkaHttpCore                   = "com.typesafe.akka"         %%  "akka-http-core"                     % AkkaVersion
  val akkaHttpCors                   = "ch.megard"                 %% "akka-http-cors"                      % "0.1.2"
  val jodaTime                       = "joda-time"                 %   "joda-time"                          % "2.9.3"

  val scalaReflect                   = "org.scala-lang"            %   "scala-reflect"                      % ScalaVersion
  val scalaXml                       = "org.scala-lang.modules"    %%  "scala-xml"                          % "1.0.5"
  val json4sJackson                  = "org.json4s"                %% "json4s-jackson"                      % "3.3.0"

  val scalikejdbc                    = "org.scalikejdbc"           %% "scalikejdbc"                         % "2.4.1"
  val scalikejdbcConfig              = "org.scalikejdbc"           %% "scalikejdbc-config"                  % "2.4.1"
  val mariadbConnector               = "org.mariadb.jdbc"          % "mariadb-java-client"                  % "1.4.6"
  val commonsPool                    = "commons-pool"              % "commons-pool"                         % "1.6"
  val commonsDbcp                    = "commons-dbcp"              % "commons-dbcp"                         % "1.4"
  val h2Connector                    = "com.h2database"            % "h2"                                   % "1.4.192"


  def compile   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")
  def provided  (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "provided")
  def test      (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")
  def runtime   (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")
  def container (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "container")
  def optional  (deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile,optional")
}
