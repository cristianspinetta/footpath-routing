import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

object Settings extends Version {

  lazy val basicSettings: Seq[Setting[_]] = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion := ScalaVersion,
    resolvers ++= Dependencies.resolutionRepos,
    version <<= version in ThisBuild,
    fork in run    := true,
    javaOptions in run := Seq(
      "-Duser.timezone=GMT-0",
      "-DenvironmentOverride=./environment-override.conf",
      "-Denvironment=dev",
      "-Dcom.sun.management.jmxremote.ssl=false",
      "-Dcom.sun.management.jmxremote.authenticate=false",
      "-Dcom.sun.management.jmxremote.port=29290",
      "-Xms1024m",
      "-Xmx2048m",
      "-verbose:gc",
      "-XX:+PrintGCDetails",
      "-XX:+PrintGCTimeStamps",
      "-XX:+PrintGCDateStamps",
      "-Xloggc:./gc.log",
      "-XX:+HeapDumpOnOutOfMemoryError",
      "-XX:HeapDumpPath=./dumps/heap-dump.hprof",
      "-XX:-OmitStackTraceInFastThrow",
      "-XX:+DisableExplicitGC",
      "-XX:+TieredCompilation",
      "-XX:+UseConcMarkSweepGC",
      "-XX:CMSInitiatingOccupancyFraction=40",
      "-XX:+UseCMSInitiatingOccupancyOnly",
      "-XX:+CMSScavengeBeforeRemark",
      "-XX:NewRatio=1"
    ),
    javacOptions   := Seq(
      "-Xlint:-options",
      "-source", JavaVersion, "-target", JavaVersion),
    scalacOptions  := Seq(
      "-encoding",
      "utf8",
      "-g:vars",
      "-feature",
      "-unchecked",
      "-optimise",
      "-deprecation",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-Yinline-warnings",
      "-Xlog-reflective-calls"
    )
  )

  lazy val assemblySettings = Assembly.settings
  lazy val notAggregateInAssembly = Assembly.notAggregateInAssembly

  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test := formattingPreferences
  )

  lazy val playgroundSettings: Seq[Setting[_]] = Seq(
    connectInput in run := true,
    cancelable in Global := true
  )

  lazy val BenchmarkConfig = Benchmark.BenchConfig

  lazy val benchmarkSettings = Benchmark.settings

  def formattingPreferences =
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, true)
      .setPreference(AlignParameters, false)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
}
