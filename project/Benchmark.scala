import sbt.Keys._
import sbt._

object Benchmark {

  lazy val BenchConfig = config("bench") extend Test

  lazy val settings: Seq[Setting[_]] = Seq(
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in BenchConfig := false,
    logBuffered := false
  )
}
