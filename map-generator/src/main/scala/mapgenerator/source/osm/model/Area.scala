package mapgenerator.source.osm.model

import scala.annotation.tailrec
import scala.collection.immutable.ListSet
import scala.collection.mutable

case class Area(parent: OSMElement, nodes: ListSet[OSMNode],
    innerRingWays: Vector[Way], outerRingWays: Vector[Way],
    innerRingNodes: List[List[Long]], outerRingNodes: List[List[Long]]) {
  //  assert(outerRingWays.nonEmpty)
  //  assert(innerRingWays.nonEmpty)

  val innerRings: List[Ring] = innerRingNodes.map(ringNodes ⇒ Ring(ringNodes, nodes))
  val outerRings: List[Ring] = outerRingNodes.map(ringNodes ⇒ Ring(ringNodes, nodes))

  val outermostRings: Vector[Ring] = {

    val result: List[Ring] = for {
      ring ← outerRings if !outerRings.exists(possibleContainer ⇒ ring != possibleContainer && ring.geometry.hasPointInside(possibleContainer.geometry))
    } yield {

      val holes = innerRings.filter(innerRing ⇒ innerRing.geometry.hasPointInside(ring.geometry))

      ring.copy(holes = holes.toVector)
    }

    result.toVector
  }

}

object Area {

  type MultiMapWithWay = mutable.ListMap[Long, mutable.Set[Way]] with mutable.MultiMap[Long, Way]

  def apply(parent: OSMElement, outerRingWays: Vector[Way], innerRingWays: Vector[Way],
    nodes: ListSet[OSMNode]): Option[Area] = {
    val innerRingNodes: List[List[Long]] = constructRings(innerRingWays)
    val outerRingNodes: List[List[Long]] = constructRings(outerRingWays)
    //    if (innerRingNodes.nonEmpty && outerRingNodes.nonEmpty)
    Some(new Area(parent, nodes, innerRingWays, outerRingWays, innerRingNodes, outerRingNodes))
    //    else
    //    None
  }

  private def constructRings(ways: Vector[Way]): List[List[Long]] = {
    if (ways.isEmpty) List.empty
    else {
      val closedRings: mutable.Builder[List[Long], List[List[Long]]] = List.newBuilder[List[Long]]

      val waysByEndpoint: MultiMapWithWay = new mutable.ListMap[Long, mutable.Set[Way]] with mutable.MultiMap[Long, Way]
      for (way ← ways) {
        val start: Long = way.nodeIds.head
        val end: Long = way.nodeIds.last
        if (start == end) {
          closedRings += way.nodeIds
        } else {
          waysByEndpoint addBinding (start, way)
          waysByEndpoint addBinding (end, way)
        }
      }

      // TODO: aca habia un precheck para situaciones raras que no implementamos.

      // FIXME implementar!
      closedRings.result()
      //      if (waysByEndpoint.isEmpty)
      //        closedRings.result()
      //      else {
      //
      //        def createRing(enpoint: Long,
      //          waysByEndpoint: MultiMapWithWay,
      //          partialRings: List[Long] = List.empty): Option[List[Long]] = {
      //          assume(partialRings.headOption.forall(_ == enpoint), "the provided endpoint must be equal to the first node in the Partial Ring")
      //
      //          waysByEndpoint.get(enpoint).flatMap { was ⇒
      //
      //            val way = ways.head
      //            val nodeIds = way.nodeIds
      //            val firstEndpoint = nodeIds.head
      //            val otherEndpoint = nodeIds.last
      //
      //            waysByEndpoint.removeBinding(firstEndpoint, way)
      //            waysByEndpoint.removeBinding(otherEndpoint, way)
      //
      //            val (newPartialRings, newFirstEndpoint): (List[Long], Long) =
      //              if (partialRings.headOption.contains(firstEndpoint))
      //                (nodeIds.reverse ::: partialRings, otherEndpoint)
      //              else
      //                (nodeIds ::: partialRings, firstEndpoint)
      //
      //            if (newPartialRings.head == newPartialRings.last)
      //              Some(newPartialRings)
      //            else
      //              createRing(newFirstEndpoint, waysByEndpoint, newPartialRings)
      //          }
      //
      //        }
      //
      //        def processAllRings(waysByEndpoint: MultiMapWithWay): List[List[Long]] = {
      //          @tailrec
      //          def iter(expression: MultiMapWithWay, acc: List[List[Long]] = Nil): List[List[Long]] = {
      //            expression.toList match {
      //              case (nodeId, _) :: wbeTail ⇒
      //                //                createRing(nodeId, waysByEndpoint).toList ::: processAllRings(waysByEndpoint)
      //                iter(expression, acc ::: createRing(nodeId, expression).toList)
      //              case _ ⇒ acc
      //            }
      //          }
      //          iter(waysByEndpoint, Nil)
      //          //          waysByEndpoint.toList match {
      //          //            case (nodeId, _) :: wbeTail ⇒
      //          //              createRing(nodeId, waysByEndpoint).toList ::: processAllRings(waysByEndpoint)
      //          //            case _ ⇒ List.empty
      //          //          }
      //        }
      //
      //        processAllRings(waysByEndpoint)
      //      }
    }
  }
}
