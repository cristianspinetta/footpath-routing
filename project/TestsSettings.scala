import sbt.Keys._
import sbt._

object TestsSettings {

  lazy val DBTestsConfig = config("dbt") extend Test

  lazy val testsSettings: Seq[Setting[_]] = Seq(
    testOptions in Test := Seq(Tests.Filter(unitFilter)),
    testOptions in DBTestsConfig := Seq(Tests.Filter(dbFilter)),
    parallelExecution in DBTestsConfig := false
  )

  private def dbFilter(name: String): Boolean = name endsWith "DBSpec"
  private def unitFilter(name: String): Boolean = !dbFilter(name)
}
