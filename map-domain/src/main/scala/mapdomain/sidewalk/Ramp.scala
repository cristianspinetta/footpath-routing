package mapdomain.sidewalk

import mapdomain.graph.Coordinate

case class Ramp(coordinate: Coordinate,
  id: String,
  street: String,
  number: Option[Int],
  address: String)
