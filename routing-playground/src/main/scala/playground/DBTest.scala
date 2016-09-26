package playground

import akka.util.ConcurrentMultiMap
import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph.Coordinate
import mapdomain.utils.GraphUtils
import playground.model.{ GeoNode, GeoNodeRepository }
import scalikejdbc.DB
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import scalikejdbc.config.DBs

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.concurrent.{ Await, Future }
import scala.util.Random

object DBTest extends App with LazyLoggerSupport with MeterSupport {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  GlobalSettings.loggingSQLAndTime = new LoggingSQLAndTimeSettings(
    enabled = true,
    singleLineMode = true,
    logLevel = 'DEBUG)

  DBs.setupAll()
  //  GeoNode.createTable3

  val from = Coordinate(-34.614665, -58.404873)
  val distance: Double = 0.5 // km

  val r = Random
  def randomCoordinate(r: Random): Coordinate = Coordinate(r.nextDouble() * 0.2 - 34.7, r.nextDouble() * 0.2 - 58.5)
  def randomDistance(r: Random): Double = r.nextDouble() * distance + 0.0000001

  write(1)

  def read() = {
    trait TypeTest
    case object ScanOnly extends TypeTest
    case object ScanWithBounding extends TypeTest

    val times = new TrieMap[TypeTest, Vector[Long]]

    val concurrency = 10
    val number = 240
    val timeout = 120 seconds

    def testScanOnly(from: Coordinate, distance: Double): Unit = {
      val scanOnly = withTimeLogging(GeoNodeRepository.findNearestWithRangeScanOnly(from, distance), { timing: Long ⇒
        logger.info(s"Fetch rows from Geo Node table by Range Scan only in $timing ms.")
        times.put(ScanOnly, times.get(ScanOnly).toVector.flatten :+ timing)
      })
      printResult("scanOnly", scanOnly)
    }

    def testScanWithBounding(from: Coordinate, distance: Double): Unit = {
      val selectWithFilter = withTimeLogging(GeoNodeRepository.findNearestWithRangeScanAndBounding(from, distance), { timing: Long ⇒
        logger.info(s"Fetch rows from Geo Node table by Range Scan with Filter in $timing ms.")
        times.put(ScanWithBounding, times.get(ScanWithBounding).toVector.flatten :+ timing)
      })
      printResult("selectWithBounding", selectWithFilter)
    }

    def printResult(testName: String, list: List[GeoNode]): Unit = {
      logger.info(s"Fetch ${list.size} rows for $testName.")
    }

    val testCases: List[() ⇒ Unit] = List(
      () ⇒ testScanOnly(randomCoordinate(r), randomDistance(r)),
      () ⇒ testScanWithBounding(randomCoordinate(r), randomDistance(r)))

    def executeTestCase(testCaseF: () ⇒ Unit): IndexedSeq[Future[IndexedSeq[Unit]]] = (1 to concurrency).map(_ ⇒
      Future {
        for (_ ← 1 to (number / concurrency))
          yield testCaseF()
      })

    //  val testCasesFut = Future.sequence(testCases.flatMap(f => executeTestCase(f)))

    testCases foreach { testCaseF ⇒
      Await.result(Future.sequence(executeTestCase(testCaseF)), timeout)
    }

    val (averageScanOnly: Double, quantityScanOnly: Int) = {
      val values = times.get(ScanOnly).toVector.flatten
      (values.sum.toDouble / values.size, values.size)
    }
    val (averageScanWithBounding: Double, quantityScanWithBounding: Int) = {
      val values = times.get(ScanWithBounding).toVector.flatten
      (values.sum.toDouble / values.size, values.size)
    }

    logger.info(s"scan only average: $averageScanOnly ms for $quantityScanOnly times")
    logger.info(s"scan with bounding average: $averageScanWithBounding ms for $quantityScanWithBounding times")

  }

  def write(quantity: Int = 1) = {

    generateData()

    def generateData(): Unit = {
      val dataSet: Vector[Coordinate] = (for (_ ← 1 to quantity) yield randomCoordinate(r)).toVector

      withTimeLogging(saveCoordinate(dataSet), { timing: Long ⇒
        logger.info(s"Inserted ${dataSet.size} rows to Geo Node table in $timing ms.")
      })
    }

    def saveCoordinate(dataSet: Vector[Coordinate]): Unit = {
      dataSet.par
        .map(coordinate ⇒ GeoNodeRepository.create(GeoNode(coordinate)))
    }

    def createGrid(rows: Int, columns: Int): List[GeoNode] = {
      val graph = GraphUtils.createGridGeoGraph(rows, columns)
      val nodes = graph.vertices.par
        .map(vertex ⇒ {
          GeoNodeRepository.create(GeoNode(vertex.coordinate))
        }).seq.toList
      nodes
    }

    def newNode(geoNode: GeoNode): Unit = {
      val newGeoNode: GeoNode = GeoNodeRepository.create(geoNode)
      logger.info(s"Create a geonoe: $newGeoNode")
    }
  }

  DBs.closeAll()
}
