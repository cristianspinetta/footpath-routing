package mapdomain.utils

import mapdomain.graph._
import org.scalameter.Bench
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object GraphUtilsBench extends Bench[Double] {

  /* configuration */

  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.max[Double],
    measurer
  )
  lazy val measurer = new Measurer.Default
//  override def measurer = new Executor.Measurer.MemoryFootprint
  lazy val reporter = new LoggingReporter[Double]
  lazy val persistor = Persistor.None

  /* inputs */

  protected val geoGraph20: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(20, 20, 0)
  protected val geoGraph10: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(10, 10, 20 * 20 + 1)
  protected val geoGraph15: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(15, 15, 20 * 20 + 1 + 10 * 10 + 1)
  protected val geoGraph3: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(3, 3, 20 * 20 + 1 + 10 * 10 + 1 + 15 * 15 + 1)
  protected val geoGraph50: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph                                                                                                                                                         (50, 50, 20 * 20 + 1 + 10 * 10 + 1 + 15 * 15 + 1 + 3 * 3 + 1)

  val geoGraph = EagerGraphContainer
    .joinGraphs[GeoVertex, EagerGeoGraphContainer[GeoVertex]](List(geoGraph20, geoGraph10, geoGraph15, geoGraph3, geoGraph50), EagerGeoGraphContainer.apply)

  val graphGen: Gen[Unit] = Gen.unit("a bigger graph")
  val graphGenBetter: Gen[Unit] = Gen.unit("a BETTER bigger graph")

  /* tests */

  performance of "Get Connected Graph" in {
    performance of "GraphUtils" in {
      measure method "getConnectedComponent" in {
        using(graphGen) in {
          _ ⇒ {
            GraphUtils.getConnectedComponent[GeoVertex, EagerGeoGraphContainer[GeoVertex]](geoGraph, EagerGeoGraphContainer.apply)
          }
        }
      }
    }
  }

}
