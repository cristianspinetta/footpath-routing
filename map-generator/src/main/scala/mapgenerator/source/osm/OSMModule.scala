package mapgenerator.source.osm

import pathgenerator.graph._

import scala.collection.mutable.ListBuffer

case class OSMModule(nodes: Seq[OSMNode], ways: Seq[Way]) {

  private val intersectionNodes: List[Long] = getIntersections(ways)

  private val createdOsmVertex = ListBuffer.empty[OsmVertex]

  def createGraph: GraphContainer[OsmVertex] = {
    ways foreach { way =>
      // TODO agregar properties utiles del way
      // TODO agregar permisos
      if (isRoutableWay(way)) {

        val wayNodes: List[Option[OSMNode]] = way.nodeIds.map(nodeId => nodes.find(node => node.id == nodeId))
        if (wayNodes.forall(_.isDefined)) {
          val wayUniqueNodes: List[OSMNode] = getUniqueNodes(wayNodes.map(_.get))

          var startNode: Option[Long] = None
          var osmStartNodeOpt: Option[OSMNode] = None
          var segmentCoordinates: ListBuffer[Coordinate] = ListBuffer.empty

          var startEndpointOpt: Option[OsmVertex] = None
          var endEndpointOpt: Option[OsmVertex] = None

          for {
            (vector, index) <- wayUniqueNodes.sliding(2).toList.zipWithIndex
          } yield {

            val firstNode = vector.head
            val secondNode = vector.tail.head

            osmStartNodeOpt = osmStartNodeOpt orElse Some(firstNode)
            startNode = startNode orElse Some(firstNode.id)

            val osmStartNode: OSMNode = osmStartNodeOpt.get

            val osmEndNode: OSMNode = secondNode

            if (segmentCoordinates.isEmpty) segmentCoordinates += Coordinate(osmStartNode.lat, osmStartNode.lon)

            segmentCoordinates += Coordinate(osmEndNode.lat, osmEndNode.lon)

            if(intersectionNodes.contains(osmEndNode) ||
              wayUniqueNodes.take(index).contains(firstNode) || // TODO checkear porque parece que esto siempre da false dado que ya quitamos los repetidos antes
              secondNode.tags.get("ele").isDefined ||
              secondNode.isStop ||
              secondNode.isBollard
            ) {
              // TODO Crear geometry (lista de coordenadas para este segmento)
              segmentCoordinates.clear()

              startEndpointOpt match {
                case None =>
                  startEndpointOpt = Some(createGraphVertex(way, osmStartNode))
                // TODO chequear si necesitamos elevation data (linea 628)
                case Some(startEndpoint) =>
                  startEndpointOpt = endEndpointOpt
              }

              endEndpointOpt = Some(createGraphVertex(way, osmEndNode))

              // TODO chequear si necesitamos elevation data (linea 640)

              val (frontEdge, backEdge) = createEdgesForStreet(startEndpointOpt.get, endEndpointOpt.get, way, osmStartNode, osmEndNode)

              addEdgeToCreatedVertex(startEndpointOpt.get, frontEdge)
              addEdgeToCreatedVertex(startEndpointOpt.get, backEdge)

              addEdgeToCreatedVertex(endEndpointOpt.get, frontEdge)
              addEdgeToCreatedVertex(endEndpointOpt.get, backEdge)

              startNode = Some(secondNode.id)
              osmStartNodeOpt = Some(secondNode)

            }



          }

        }
      }
    }
    GraphContainer(createdOsmVertex.toList)
  }

  private def addEdgeToCreatedVertex(osmVertex: OsmVertex, osmEdge: OsmStreetEdge): Unit = {

    val indexWhere: Int = createdOsmVertex.indexWhere(vertex => vertex.id == osmVertex.id)

    val foundOsmVertex: OsmVertex = createdOsmVertex(indexWhere)

    val updatedOsmVertex: OsmVertex = foundOsmVertex match {
      case vertex: TransitStopStreetVertex => vertex.copy(edges = osmEdge :: osmVertex.edges)
      case vertex: ExitVertex => vertex.copy(edges = osmEdge :: osmVertex.edges)
      case vertex: BarrierVertex => vertex.copy(edges = osmEdge :: osmVertex.edges)
      case vertex: OsmVertex => new OsmVertex(vertex.id, osmEdge :: osmVertex.edges, vertex.coordinate)
    }

    createdOsmVertex.update(indexWhere, updatedOsmVertex)

  }

  private def createEdgesForStreet(startEndpoint: OsmVertex, endEndpoint: OsmVertex, way: Way, osmStartNode: OSMNode, osmEndNode: OSMNode): (OsmStreetEdge, OsmStreetEdge) = {
    // TODO implementar permissions.allowsNothing (se usa en linea 1006)
    // TODO LineString backGeometry (linea 1010)
    // TODO implementar: "double length = this.getGeometryLengthMeters(geometry);" (linea 1012)
    // TODO implementar: "P2<StreetTraversalPermission> permissionPair = OSMFilter.getPermissions(permissions, way);" (linea 1014)

    // TODO if (permissionsFront.allowsAnything()) {
    val front = createOsmStreetEdge(startEndpoint, endEndpoint, way)
    // TODO if (permissionsBack.allowsAnything()) {
    val back = createOsmStreetEdge(endEndpoint, startEndpoint, way)

    // TODO revisar el shareData() y el way.isRoundabout() (linea 1028 y 1032)

    (front, back)

  }

  private def createOsmStreetEdge(startEndpoint: OsmVertex, endEndpoint: OsmVertex, way: Way): OsmStreetEdge = {
    // TODO crear label y name (linea 1046)
    // TODO crear length a partir del recorrido de los coordinate (1053)

    // TODO revisar implementacion de cls (linea 1078)}
    // TODO revisar wheelChairAccesible (linea 1087)
    // TODO revisar slope override (linea 1092)
    // TODO revisar todos los datos extra que hay del way, por ejemplo la "car speed"

    OsmStreetEdge(startEndpoint, endEndpoint, 10)

    // TODO revisar el resto de las cosas de este metodo (linea 1092 en adelante)
  }

  private def createGraphVertex(way: Way, osmStartNode: OSMNode): OsmVertex = {
    if (osmStartNode.isMultiLevel) {
      // TODO
      ???
    } else {
      createdOsmVertex.find(_.id == osmStartNode.id) match {
        case None =>
          // TODO implementar el label

          val vertex: OsmVertex = osmStartNode match {
            case node if node.tags.get("highway").contains("motorway_junction") && node.tags.get("ref").isDefined =>
              ExitVertex(node.id, Nil, Coordinate(node.lat, node.lon), node.tags("ref"))
            case node if node.isStop && node.tags.get("ref").isDefined =>
              TransitStopStreetVertex(node.id, Nil, Coordinate(node.lat, node.lon)) // TODO chequear loas demas datos que le agregan (linea 1192 de OSMModule)
            case node if node.isBollard =>
              BarrierVertex(node.id, Nil, Coordinate(node.lat, node.lon)) // TODO chequear los permisos que le agregan (linea 1199)
            case node =>
              new OsmVertex(node.id, Nil, Coordinate(node.lat, node.lon))
          }

          createdOsmVertex += vertex
          vertex
        case Some(vertex) => vertex
      }
    }
  }

  private def isRoutableWay(way: Way): Boolean = {
    // TODO implementar!!
    true
  }

  // TODO analizar bien si necesitamos este metodo, y para que se usa
  private def getUniqueNodes(nodes: List[OSMNode]): List[OSMNode] = {
    var lastNodeId: Option[Long] = None
    var lastLat: Option[Double] = None
    var lastLon: Option[Double] = None
    var lastLevel: Option[String] = None
    nodes flatMap { node =>
      val level: Option[String] = node.tags.get("level")
      val levelsDiffer: Boolean = level != lastLevel
      val result = if (!lastNodeId.contains(node.id) &&
        (!lastLat.contains(node.lat) || !lastLon.contains(node.lon) || levelsDiffer))
        List(node)
      else
        Nil

      lastNodeId = Some(node.id)
      lastLat = Some(node.lat)
      lastLon = Some(node.lon)
      lastLevel = level

      result
    }
  }

  private def getIntersections(ways: Seq[Way]): List[Long] = {
    var possibleIntersectionNodes = ListBuffer.empty[Long]
    var intersectionNodes = ListBuffer.empty[Long]
    ways foreach { way =>
      way.nodeIds foreach {nodeId =>
        if (possibleIntersectionNodes.contains(nodeId))
          intersectionNodes += nodeId
        else
          possibleIntersectionNodes += nodeId
      }
    }
    intersectionNodes.toList
    // Functional way:
//    val (_, intersectionNodesF) = ways
//      .flatMap(_.nodeIds)
//      .foldLeft((List.empty[Long], List.empty[Long])) { case ((possibleIntersectionNodes, accIntersectionNodes), nodeId) =>
//        if (possibleIntersectionNodes.contains(nodeId))
//          (possibleIntersectionNodes, nodeId :: accIntersectionNodes)
//        else
//          (nodeId :: possibleIntersectionNodes, accIntersectionNodes)
//      }
//
//    intersectionNodesF
  }
}

class OsmVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends GeoVertex(id, edges, coordinate)

case class TransitStopStreetVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)
case class ExitVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate, exitName: String) extends OsmVertex(id, edges, coordinate)
case class BarrierVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)

case class OsmStreetEdge(osmVertexStart: OsmVertex, osmVertexEnd: OsmVertex, override val distance: Double)
  extends GeoEdge(osmVertexStart.id, osmVertexEnd.id, distance, directed = true)
