package mapgenerator.source.osm.graph

import pathgenerator.graph.Coordinate

case class Ramp(coordinate: Coordinate,
  id: String,
  street: String,
  number: Option[Int],
  address: String)
