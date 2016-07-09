package mapgenerator.source.osm.model

import mapgenerator.source.osm.graph.Polygon

import scala.collection.immutable.ListSet

case class Ring(nodeIds: List[Long], nodes: ListSet[OSMNode], holes: Vector[Ring] = Vector.empty) {
  val geometry: Polygon = Polygon(nodes.map(_.coordinate).toVector)
}
