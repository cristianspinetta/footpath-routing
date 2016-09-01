package mapgenerator.sidewalk

import base.LazyLoggerSupport
import mapdomain.graph.{ GeoEdge, GeoVertex }
import mapdomain.sidewalk.{ PedestrianEdge, SidewalkEdge, SidewalkVertex, StreetCrossingEdge }

import scala.collection.Map

case class Builders[V <: GeoVertex](
  streetCrossingBuilderManager: StreetCrossingBuilderManager,
  sidewalkVertexBuilderManager: SidewalkVertexBuilderManager,
  sidewalkEdgeBuilderManager: SidewalkEdgeBuilderManager[V])

object SideWalkBuilder extends LazyLoggerSupport {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  def build[V <: GeoVertex](failureTolerance: Boolean = false)(implicit idGenerator: SidewalkVertexIDGenerator, builders: Builders[V]): Set[SidewalkVertex] = {

    val sidewalkEdgeBuilders: Iterable[SidewalkEdgeBuilder] = builders.sidewalkEdgeBuilderManager.sidewalkOnCornerByKey.values

    val buildSidewalkWithToleranceFailure: ((Set[SidewalkEdge], Set[SidewalkVertex]), SidewalkEdgeBuilder) ⇒ (Set[SidewalkEdge], Set[SidewalkVertex]) = {
      case ((swEdges, vs), builder) ⇒
        builder.buildFailureTolerance match {
          case Some((edge: SidewalkEdge, vertexStart: SidewalkVertex, vertexEnd: SidewalkVertex)) ⇒ (swEdges + edge, vs + vertexStart + vertexEnd)
          case None ⇒ (swEdges, vs)
        }
    }
    val buildSidewalkWithNoToleranceFailure: ((Set[SidewalkEdge], Set[SidewalkVertex]), SidewalkEdgeBuilder) ⇒ (Set[SidewalkEdge], Set[SidewalkVertex]) = {
      case ((swEdges, vs), builder) ⇒
        val (edge: SidewalkEdge, vertexStart: SidewalkVertex, vertexEnd: SidewalkVertex) = builder.build
        (swEdges + edge, vs + vertexStart + vertexEnd)
    }

    val (sidewalkEdges: Set[SidewalkEdge], verticesFromSidewalk: Set[SidewalkVertex]) = {
      val fold = sidewalkEdgeBuilders.foldLeft((Set[SidewalkEdge](), Set[SidewalkVertex]())) _
      if (failureTolerance) fold(buildSidewalkWithToleranceFailure)
      else fold(buildSidewalkWithNoToleranceFailure)
    }

    val (streetCrossingEdges: Set[StreetCrossingEdge], verticesFromCrossing: Set[SidewalkVertex]) = builders.streetCrossingBuilderManager.builders
      .foldLeft((Set[StreetCrossingEdge](), Set[SidewalkVertex]())) {
        case ((scEdges, vs), builder) ⇒
          val (edge: StreetCrossingEdge, vertexFrom: SidewalkVertex, vertexTo: SidewalkVertex) = builder.build
          (scEdges + edge, vs + vertexFrom + vertexTo)
      }

    val vertices: Set[SidewalkVertex] = verticesFromSidewalk ++ verticesFromCrossing

    val verticesWithSidewalks: Set[SidewalkVertex] = introduceEdges[SidewalkEdge](vertices, sidewalkEdges.toList,
      (vertex, edge) ⇒ vertex.copy(sidewalkEdges = edge :: vertex.sidewalkEdges))

    val verticesWithSidewalksAndCrossing: Set[SidewalkVertex] = introduceEdges[StreetCrossingEdge](verticesWithSidewalks,
      streetCrossingEdges.toList, (vertex, edge) ⇒ vertex.copy(streetCrossingEdges = edge :: vertex.streetCrossingEdges))

    verticesWithSidewalksAndCrossing
  }

  private def uniqueEdges[E <: PedestrianEdge](vertices: Set[SidewalkVertex], edges: List[E]): Map[SidewalkVertex, Set[E]] = {
    vertices.map(vertex ⇒ {
      val filteredEdges: Set[E] = edges.filter(edge ⇒ edge.vertexStart == vertex.id || edge.vertexEnd == vertex.id).toSet
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
