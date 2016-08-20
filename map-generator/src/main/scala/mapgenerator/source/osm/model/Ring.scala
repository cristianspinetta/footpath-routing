package mapgenerator.source.osm.model

import mapdomain.graph.Coordinate
import mapdomain.math.Polygon

import scala.collection.immutable.ListSet
import scala.collection.mutable.ArrayBuffer

case class Ring(nodes: ListSet[OSMNode], vertices: Vector[Coordinate], holes: Vector[Ring] = Vector.empty) {
  val geometry: Polygon = Polygon(vertices)
}

object Ring {
  def apply(nodeIds: List[Long], nodes: ListSet[OSMNode]): Ring = {

    val processedNodes = ArrayBuffer.empty[Long]

    val ringNodes: ListSet[OSMNode] = ListSet((for {
      nodeId ← nodeIds if !processedNodes.contains(nodeId)
    } yield {
      val node: OSMNode = nodes.find(_.id == nodeId).get
      processedNodes += nodeId
      node
    }): _*)

    val vertices: Vector[Coordinate] = nodes.map(_.coordinate).toVector

    new Ring(ringNodes, vertices)
  }
}
