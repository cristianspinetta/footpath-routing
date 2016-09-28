package mapgenerator.sidewalk

import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph.{GeoEdge, GeoVertex}
import mapdomain.sidewalk.{ PedestrianEdge, SidewalkEdge, SidewalkVertex, StreetCrossingEdge }

import scala.collection.Map
import scala.collection.mutable

case class Builders[E <: GeoEdge, V <: GeoVertex[E]](
  streetCrossingBuilderManager: StreetCrossingBuilderManager,
  sidewalkVertexBuilderManager: SidewalkVertexBuilderManager,
  sidewalkEdgeBuilderManager: SidewalkEdgeBuilderManager[E, V])

object SideWalkBuilder extends LazyLoggerSupport with MeterSupport {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  def build[V <: GeoVertex[_]](failureTolerance: Boolean = false)
                           (implicit idGenerator: SidewalkVertexIDGenerator, builders: Builders[_, V]): Set[SidewalkVertex] = withTimeLogging({

    type BuildSidewalkFunc = ((mutable.Set[SidewalkEdge], mutable.Set[SidewalkVertex]), SidewalkEdgeBuilder) ⇒ Unit

    logger.info(s"Finally getting started to building the sidewalks as vertices and edges.")

    val sidewalkEdgeBuilders: Vector[SidewalkEdgeBuilder] = builders.sidewalkEdgeBuilderManager.sidewalkOnCornerByKey.values.toVector

    val buildSidewalkWithToleranceFailure: BuildSidewalkFunc = {
      case ((swEdges, vs), builder) ⇒
        builder
          .buildFailureTolerance
          .map { case (edge: SidewalkEdge, vertexStart: SidewalkVertex, vertexEnd: SidewalkVertex) ⇒
            swEdges += edge
            vs += vertexStart
            vs += vertexEnd
        }
    }
    val buildSidewalkWithNoToleranceFailure: BuildSidewalkFunc = {
      case ((swEdges, vs), builder) ⇒
        val (edge: SidewalkEdge, vertexStart: SidewalkVertex, vertexEnd: SidewalkVertex) = builder.build
        swEdges += edge
        vs += vertexStart
        vs += vertexEnd
    }

    logger.info(s"Starting to build sidewalk edges...")

    val (sidewalkEdges: Set[SidewalkEdge], verticesFromSidewalk: Set[SidewalkVertex]) = {

      val sidewalkEdgeSet: mutable.Set[SidewalkEdge] = mutable.Set()
      val sidewalkVertexSet: mutable.Set[SidewalkVertex] = mutable.Set()

      def processSidewalk(buildSidewalkFunc: BuildSidewalkFunc): Unit = {
        sidewalkEdgeBuilders foreach { sidewalkEdgeBuilder =>
          buildSidewalkFunc((sidewalkEdgeSet, sidewalkVertexSet), sidewalkEdgeBuilder)
        }
      }

      if (failureTolerance) processSidewalk(buildSidewalkWithToleranceFailure)
      else processSidewalk(buildSidewalkWithNoToleranceFailure)

      (sidewalkEdgeSet.toSet, sidewalkVertexSet.toSet)
    }

    logger.info(s"Starting to build street crossing edges...")

    val (streetCrossingEdges: Set[StreetCrossingEdge], verticesFromCrossing: Set[SidewalkVertex]) = {

      val streetCrossingEdges = mutable.Set[StreetCrossingEdge]()
      val sidewalkVertices = mutable.Set[SidewalkVertex]()

      builders.streetCrossingBuilderManager.builders foreach { builder =>
        val (edge: StreetCrossingEdge, vertexFrom: SidewalkVertex, vertexTo: SidewalkVertex) = builder.build
        streetCrossingEdges += edge
        sidewalkVertices += vertexFrom
        sidewalkVertices += vertexTo
      }

      (streetCrossingEdges.toSet, sidewalkVertices.toSet)
    }

    logger.info(s"Starting to build sidewalk vertices...")

    val vertices: Set[SidewalkVertex] = verticesFromSidewalk ++ verticesFromCrossing

    val verticesWithSidewalks: Set[SidewalkVertex] = introduceEdges[SidewalkEdge](vertices, sidewalkEdges.toList,
      (vertex, edge) ⇒ vertex.copy(sidewalkEdges = edge :: vertex.sidewalkEdges))

    val verticesWithSidewalksAndCrossing: Set[SidewalkVertex] = introduceEdges[StreetCrossingEdge](verticesWithSidewalks,
      streetCrossingEdges.toList, (vertex, edge) ⇒ vertex.copy(streetCrossingEdges = edge :: vertex.streetCrossingEdges))

    verticesWithSidewalksAndCrossing
  }, (time: Long) => logger.info(s"Finish the 'Vertices and Edges Sidewalk Building' phase in $time ms."))

  private def uniqueEdges[E <: PedestrianEdge](vertices: Set[SidewalkVertex], edges: List[E]): Map[SidewalkVertex, Set[E]] = {
    vertices.map(vertex ⇒ {
      val filteredEdges: Set[E] = edges.filter(edge ⇒ edge.vertexStartId == vertex.id || edge.vertexEndId == vertex.id).toSet
      vertex -> filteredEdges
    }).toMap
  }

  private def introduceEdges[E <: PedestrianEdge](vertices: Set[SidewalkVertex], edges: List[E],
                                                  copyF: (SidewalkVertex, E) ⇒ SidewalkVertex): Set[SidewalkVertex] = {
    val verticesMap: Map[SidewalkVertex, Set[E]] = uniqueEdges(vertices, edges)
    val finalVertices = for {
      (vertex, edgeSet) ← verticesMap
    } yield edgeSet.foldLeft(vertex)((partialVertex, edge) ⇒ copyF(partialVertex, edge))
    finalVertices.toSet
  }
}
