package mapdomain.publictransport

import mapdomain.graph.{ Coordinate, CoordinateRepository }

case class TransportationInfo(id: Int, lineNro: Int)

case class PathTP(id: Long,
  path: List[Coordinate],
  transportationInfo: Option[TransportationInfo] = None)

object PathTP {
  def apply(paths: List[List[(Double, Double)]]): List[PathTP] =
    paths.zip(1 to paths.size) map {
      case (coordinates, id) ⇒
        PathTP(id, coordinates.map(c ⇒ CoordinateRepository.create(c._1, c._2)))
    }
}
