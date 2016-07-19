package mapgenerator.source.osm

import enums.StreetTraversalPermission
import mapgenerator.source.osm.graph._
import mapgenerator.source.osm.model._
import pathgenerator.graph.{Coordinate, GraphContainer}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class GraphModule(osmModule: OSMModule) {

  private val intersectionNodes: Set[Long] = getIntersections
  private val createdOsmVertex = ListBuffer.empty[OsmVertex]

  def createGraph: GraphContainer[OsmVertex] = {
    for (way ← osmModule.streetWays) processStreetWay(way)
    GraphContainer(createdOsmVertex.toList)
  }

  private def processStreetWay(way: Way): Unit = {
    // TODO agregar properties utiles del way
    // TODO agregar permisos

    val permissions: StreetTraversalPermission = OSMModule.getPermissionsForWay(way, StreetTraversalPermission.ALL)

    val wayNodes: List[Option[OSMNode]] = way.nodeIds.map(nodeId ⇒ osmModule.otherNodes.find(node ⇒ node.id == nodeId))
    if (way.isRoutableWay && !permissions.allowsNothing() && wayNodes.forall(_.isDefined)) {
      val wayUniqueNodes: List[OSMNode] = getUniqueNodes(wayNodes.map(_.get))

      var startNode: Option[Long] = None
      var osmStartNodeOpt: Option[OSMNode] = None
      val segmentCoordinates: ListBuffer[Coordinate] = ListBuffer.empty

      var startEndpointOpt: Option[OsmVertex] = None
      var endEndpointOpt: Option[OsmVertex] = None

      val coupleWays: List[List[OSMNode]] = wayUniqueNodes.sliding(2).toList

      for ((vector, index) ← coupleWays.zipWithIndex) {

        val firstNode = vector.head
        val secondNode = vector.tail.head

        osmStartNodeOpt = osmStartNodeOpt orElse Some(firstNode)
        startNode = startNode orElse Some(firstNode.id)

        val osmStartNode: OSMNode = osmStartNodeOpt.get

        val osmEndNode: OSMNode = secondNode

        if (segmentCoordinates.isEmpty) segmentCoordinates += Coordinate(osmStartNode.lat, osmStartNode.lon)

        segmentCoordinates += Coordinate(osmEndNode.lat, osmEndNode.lon)

        if (intersectionNodes.contains(osmEndNode.id) ||
          coupleWays.size == index + 1 || // Last couple of ways
          wayUniqueNodes.take(index).contains(firstNode) ||
          secondNode.tags.get("ele").isDefined ||
          secondNode.isStop ||
          secondNode.isBollard) {
          // TODO Crear geometry (lista de coordenadas para este segmento)
          segmentCoordinates.clear()

          startEndpointOpt match {
            case None ⇒
              startEndpointOpt = Some(createGraphVertex(way, osmStartNode))
            // TODO chequear si necesitamos elevation data (linea 628)
            case Some(startEndpoint) ⇒
              startEndpointOpt = endEndpointOpt
          }

          endEndpointOpt = Some(createGraphVertex(way, osmEndNode))

          // TODO chequear si necesitamos elevation data (linea 640)

          val (frontEdgeOpt, backEdgeOpt) = createEdgesForStreet(startEndpointOpt.get, endEndpointOpt.get, way, osmStartNode, osmEndNode, permissions)

          frontEdgeOpt.foreach(frontEdge => addEdgeToCreatedVertex(startEndpointOpt.get, frontEdge))
          backEdgeOpt.foreach(backEdge => addEdgeToCreatedVertex(endEndpointOpt.get, backEdge))

          startNode = Some(secondNode.id)
          osmStartNodeOpt = Some(secondNode)

        }
      }
    }
  }

  private def addEdgeToCreatedVertex(osmVertex: OsmVertex, osmEdge: OsmStreetEdge): Unit = {

    assert(osmEdge.osmVertexStart == osmVertex, "The startVertex of the edge must be the Vertex that it belong to.")

    val indexWhere: Int = createdOsmVertex.indexWhere(vertex ⇒ vertex.id == osmVertex.id)

    val foundOsmVertex: OsmVertex = createdOsmVertex(indexWhere)

    val updatedOsmVertex: OsmVertex = foundOsmVertex match {
      case vertex: TransitStopStreetVertex ⇒ vertex.copy(edges = osmEdge :: foundOsmVertex.edges)
      case vertex: ExitVertex              ⇒ vertex.copy(edges = osmEdge :: foundOsmVertex.edges)
      case vertex: BarrierVertex           ⇒ vertex.copy(edges = osmEdge :: foundOsmVertex.edges)
      case vertex: OsmVertex               ⇒ new OsmVertex(foundOsmVertex.id, osmEdge :: foundOsmVertex.edges, foundOsmVertex.coordinate)
    }

    createdOsmVertex.update(indexWhere, updatedOsmVertex)

  }

  private def createEdgesForStreet(startEndpoint: OsmVertex, endEndpoint: OsmVertex, way: Way, osmStartNode: OSMNode,
                                   osmEndNode: OSMNode, permissions: StreetTraversalPermission): (Option[OsmStreetEdge], Option[OsmStreetEdge]) = {
    // TODO implementar permissions.allowsNothing (se usa en linea 1006)
    // TODO LineString backGeometry (linea 1010)
    // TODO implementar: "double length = this.getGeometryLengthMeters(geometry);" (linea 1012)

    val (permissionsFront, permissionsBack) = getPairPermissionsForWay(permissions, way)

    val frontOpt =
      if (permissionsFront.allowsAnything())
        Some(createOsmStreetEdge(startEndpoint, endEndpoint, way, permissions))
      else
        None
    val backOpt =
      if (permissionsBack.allowsAnything())
        Some(createOsmStreetEdge(endEndpoint, startEndpoint, way, permissions))
      else
        None

    // TODO revisar el shareData() y el way.isRoundabout() (linea 1028 y 1032)

    (frontOpt, backOpt)
  }

  private def createOsmStreetEdge(startEndpoint: OsmVertex, endEndpoint: OsmVertex, way: Way,
                                  permissions: StreetTraversalPermission): OsmStreetEdge = {
    // TODO crear label y name (linea 1046)
    // TODO crear length a partir del recorrido de los coordinate (1053)

    // TODO revisar implementacion de cls (linea 1078)}
    // TODO revisar wheelChairAccesible (linea 1087)
    // TODO revisar slope override (linea 1092)
    // TODO revisar todos los datos extra que hay del way, por ejemplo la "car speed"

    // TODO implementar "permissions: StreetTraversalPermission" para distinguir entre ida y vuelta

    OsmStreetEdge(startEndpoint, endEndpoint, 10, way.id)

    // TODO revisar el resto de las cosas de este metodo (linea 1092 en adelante)
  }

  private def getPairPermissionsForWay(permissions: StreetTraversalPermission, way: Way): (StreetTraversalPermission, StreetTraversalPermission) = {
    var permissionsFront: StreetTraversalPermission = permissions
    var permissionsBack: StreetTraversalPermission = permissions

    // Check driving direction restrictions.
    if (way.isOneWayForwardDriving || way.isRoundabout) {
      permissionsBack = permissionsBack.remove(StreetTraversalPermission.BICYCLE_AND_CAR)
    }
    if (way.isOneWayReverseDriving) {
      permissionsFront = permissionsFront.remove(StreetTraversalPermission.BICYCLE_AND_CAR)
    }

    // Check bike direction restrictions.
    if (way.isOneWayForwardBicycle) {
      permissionsBack = permissionsBack.remove(StreetTraversalPermission.BICYCLE)
    }
    if (way.isOneWayReverseBicycle) {
      permissionsFront = permissionsFront.remove(StreetTraversalPermission.BICYCLE)
    }

    // TODO(flamholz): figure out what this is for.
    if (OSMElement.isTagFalse(way.tags, "oneway:bicycle") || OSMElement.isTagTrue(way.tags, "bicycle:backwards")) {
      if (permissions.allows(StreetTraversalPermission.BICYCLE)) {
        permissionsFront = permissionsFront.add(StreetTraversalPermission.BICYCLE)
        permissionsBack = permissionsBack.add(StreetTraversalPermission.BICYCLE)
      }
    }

    //This needs to be after adding permissions for oneway:bicycle=no
    //removes bicycle permission when bicycles need to use sidepath
    //TAG: bicycle:forward=use_sidepath
    if (way.isForwardDirectionSidepath) {
      permissionsFront = permissionsFront.remove(StreetTraversalPermission.BICYCLE)
    }

    //TAG bicycle:backward=use_sidepath
    if (way.isReverseDirectionSidepath) {
      permissionsBack = permissionsBack.remove(StreetTraversalPermission.BICYCLE)
    }

    if (way.isOpposableCycleway) {
      permissionsBack = permissionsBack.add(StreetTraversalPermission.BICYCLE)
    }

    (permissionsFront, permissionsBack)
  }

  private def createGraphVertex(way: Way, osmStartNode: OSMNode): OsmVertex = {
    if (osmStartNode.isMultiLevel) {
      // TODO
      ???
    } else {
      createdOsmVertex.find(_.id == osmStartNode.id) match {
        case None ⇒
          // TODO implementar el label

          val vertex: OsmVertex = osmStartNode match {
            case node if node.tags.get("highway").contains("motorway_junction") && node.tags.get("ref").isDefined ⇒
              ExitVertex(node.id, Nil, Coordinate(node.lat, node.lon), node.tags("ref"))
            case node if node.isStop && node.tags.get("ref").isDefined ⇒
              TransitStopStreetVertex(node.id, Nil, Coordinate(node.lat, node.lon)) // TODO chequear loas demas datos que le agregan (linea 1192 de OSMModule)
            case node if node.isBollard ⇒
              BarrierVertex(node.id, Nil, Coordinate(node.lat, node.lon)) // TODO chequear los permisos que le agregan (linea 1199)
            case node ⇒ OsmVertex(way, node)
          }

          createdOsmVertex += vertex
          vertex
        case Some(vertex) ⇒ vertex
      }
    }
  }

  // TODO analizar bien si necesitamos este metodo, y para que se usa
  private def getUniqueNodes(nodes: List[OSMNode]): List[OSMNode] = {
    var lastNodeId: Option[Long] = None
    var lastLat: Option[Double] = None
    var lastLon: Option[Double] = None
    var lastLevel: Option[String] = None
    nodes flatMap { node ⇒
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

  private def getIntersections: Set[Long] = {
    val possibleIntersectionNodes = ArrayBuffer.empty[Long]
    val intersectionNodes = ArrayBuffer.empty[Long]
    for {
      way ← osmModule.streetWays
      nodeId ← way.nodeIds
    } {
      if (possibleIntersectionNodes.contains(nodeId))
        intersectionNodes += nodeId
      else
        possibleIntersectionNodes += nodeId
    }

    for {
      area ← osmModule.walkableAreas ++ osmModule.parkAndRideAreas
      outerRing ← area.outermostRings
      node ← outerRing.nodes
    } {
      if (possibleIntersectionNodes.contains(node.id))
        intersectionNodes += node.id
      else
        possibleIntersectionNodes += node.id
    }

    intersectionNodes.toSet
  }
}
