package mapdomain.sidewalk

import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex }
import mapdomain.math.Line
import mapdomain.sidewalk.SidewalkEdge.Side

case class SidewalkEdge(from: SidewalkVertex, to: SidewalkVertex, streetEdgeBelongTo: GeoEdge, side: Side)

object SidewalkEdge {

  def sideByEdges(streetLine: Line, sidewalkLine: Line): Side = {
    if (Line.compareParallelsByAltitude(streetLine, sidewalkLine) == 1) SouthSide
    else NorthSide
  }

  trait Side
  case object NorthSide extends Side
  case object SouthSide extends Side
}

case class SidewalkVertex(id: Long, coordinate: Coordinate, streetVertexBelongTo: GeoVertex)

