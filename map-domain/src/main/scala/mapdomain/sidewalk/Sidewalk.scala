package mapdomain.sidewalk

import base.{ FailureReporterSupport, LazyLoggerSupport }
import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex }
import mapdomain.math.Line
import mapdomain.sidewalk.SidewalkEdge.Side

case class SidewalkEdge(from: SidewalkVertex, to: SidewalkVertex, streetEdgeBelongTo: GeoEdge, side: Side)

object SidewalkEdge extends FailureReporterSupport with LazyLoggerSupport {

  def sideByEdges(streetLine: Line, sidewalkLine: Line): Side = withFailureLogging({
    if (Line.compareParallelsByAltitude(streetLine, sidewalkLine) == 1) SouthSide
    else NorthSide
  }, (exc: Throwable) ⇒ logger.error(s"Failed trying to calculate the side of a sidewalk from an edge.", exc))

  trait Side
  case object NorthSide extends Side
  case object SouthSide extends Side
}

case class SidewalkVertex(id: Long, coordinate: Coordinate, streetVertexBelongTo: GeoVertex)
