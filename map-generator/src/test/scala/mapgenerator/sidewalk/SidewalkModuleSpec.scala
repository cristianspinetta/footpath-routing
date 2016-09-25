package mapgenerator.sidewalk

import java.util.concurrent.atomic.AtomicLong

import mapdomain.graph.Coordinate
import mapdomain.sidewalk._
import mapdomain.street.{ InMemoryStreetGraphContainer, StreetEdge, StreetVertex }
import mapdomain.utils.GraphUtils
import org.scalatest.{ FlatSpec, Matchers }

class SidewalkModuleSpec extends FlatSpec with Matchers {

  val streetGraphPrototype: InMemoryStreetGraphContainer = createStreetGraph(Map(
    1L -> (List(2L, 4L), Coordinate(0, 0)),
    2L -> (List(1L, 3L, 5L), Coordinate(0, 10)),
    3L -> (List(2L, 5L, 6L), Coordinate(0, 20)),
    4L -> (List(1L, 5L, 7L), Coordinate(-10, 0)),
    5L -> (List(2L, 3L, 4L, 6L, 8L, 9L), Coordinate(-10, 10)),
    6L -> (List(3L, 5L, 9L, 10L), Coordinate(-10, 20)),
    7L -> (List(4L, 8L), Coordinate(-20, 0)),
    8L -> (List(5L, 7L, 9L), Coordinate(-20, 10)),
    9L -> (List(5L, 6L, 8L), Coordinate(-20, 20)),
    10L -> (List(6L), Coordinate(-10, 30))))

  val unconnectedStreetGraphPrototype: InMemoryStreetGraphContainer = createStreetGraph(Map(
    1L -> (List(2L, 4L), Coordinate(0, 0)),
    2L -> (List(1L, 3L, 5L), Coordinate(0, 10)),
    3L -> (List(2L, 5L, 6L), Coordinate(0, 20)),
    4L -> (List(1L, 5L, 7L), Coordinate(-10, 0)),
    5L -> (List(2L, 3L, 4L, 6L, 8L, 9L), Coordinate(-10, 10)),
    6L -> (List(3L, 5L, 9L, 10L), Coordinate(-10, 20)),
    7L -> (List(4L, 8L), Coordinate(-20, 0)),
    8L -> (List(5L, 7L, 9L), Coordinate(-20, 10)),
    9L -> (List(5L, 6L, 8L), Coordinate(-20, 20)),
    10L -> (List(6L), Coordinate(-10, 30)),
    11L -> (List(12L), Coordinate(-50, 40)),
    12L -> (List(11L, 13L), Coordinate(-50, 50)),
    13L -> (List(12L), Coordinate(-50, 60))))

  "The SidewalkModule" should "create all sidewalk for a GeoVertex Graph" in {
    val sidewalkModule = SidewalkModule()(streetGraphPrototype)
    val sidewalkGraphContainer: InMemorySidewalkGraphContainer = sidewalkModule.createSideWalks(distanceToStreet = 1)
    sidewalkGraphContainer.vertices.size should be(31)
    sidewalkGraphContainer.sidewalkEdges.size should be(30)
    sidewalkGraphContainer.streetCrossingEdges.size should be(28)

  }

  it should "get a single maximum graph that is connected" in {
    val sidewalkModule = SidewalkModule()(unconnectedStreetGraphPrototype)
    val sidewalkGraphContainer: InMemorySidewalkGraphContainer = sidewalkModule.createSideWalks(distanceToStreet = 1)
    val connectedGraph = sidewalkGraphContainer.purgeSidewalks
    connectedGraph.sidewalkEdges.size should be(30)
    connectedGraph.streetCrossingEdges.size should be(28)
    GraphUtils.isGraphConnected(connectedGraph) should be(true)

  }

  def createStreetGraph(vertexData: Map[Long, (List[Long], Coordinate)]): InMemoryStreetGraphContainer = {
    val id = new AtomicLong(0)
    val vertices: List[StreetVertex] = vertexData.toList map {
      case (nodeId, (edgeIds, nodeCoordinate)) ⇒
        new StreetVertex(nodeId,
          edgeIds.map(neighbourId ⇒ StreetEdge(Some(id.addAndGet(1)), nodeId, neighbourId, nodeCoordinate.distanceTo(vertexData(neighbourId)._2), 0, 0)),
          nodeCoordinate)
    }
    InMemoryStreetGraphContainer(vertices)
  }

  def createSidewalkGraph(vertexData: Map[Long, (List[Long], Coordinate)]): InMemorySidewalkGraphContainer = {
    val vertices: List[SidewalkVertex] = vertexData.toList map {
      case (nodeId, (edgeIds, nodeCoordinate)) ⇒
        new SidewalkVertex(nodeId, nodeCoordinate,
          edgeIds.map(neighbourId ⇒ SidewalkEdge(nodeId, neighbourId, s"fake-key-$nodeId-$neighbourId", NorthSide, None)),
          Nil, 0)
    }
    InMemorySidewalkGraphContainer(vertices)
  }
}
