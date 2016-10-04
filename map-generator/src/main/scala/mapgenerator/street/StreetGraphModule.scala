package mapgenerator.street

import base.{ LazyLoggerSupport, MeterSupport }
import enums.StreetTraversalPermission
import mapdomain.graph.Coordinate
import mapdomain.street.{ StreetEdgeUnsaved, StreetInfo, UnsavedStreetGraphContainer, UnsavedStreetVertex }
import mapgenerator.source.osm.OSMModule
import mapgenerator.source.osm.model._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer

case class StreetGraphModule(osmModule: OSMModule) extends LazyLoggerSupport with MeterSupport {

  private val intersectionNodes: Set[Long] = getIntersections
  private val createdStreetVertex = ArrayBuffer.empty[UnsavedStreetVertex]

  private val multiLevelNodes = TrieMap[Long, Map[OSMLevel, UnsavedStreetVertex]]()

  private val streetInfoByWayId = TrieMap[Long, StreetInfo]()

  def createGraph: UnsavedStreetGraphContainer = {
    logger.info("Starting to create a Street Graph from the OSM Module")
    withTimeLogging({
      osmModule.streetWays.foldLeft(0) {
        case (counter, way) ⇒
          if (counter % 1000 == 0) logger.info(s"$counter ways parsed.")
          processStreetWay(way)
          counter + 1
      }
      UnsavedStreetGraphContainer(createdStreetVertex.toList)
    }, (time: Long) ⇒ logger.info(s"Created Street Graph in $time ms."))
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
      val segmentCoordinates: ArrayBuffer[Coordinate] = ArrayBuffer.empty

      var startEndpointOpt: Option[UnsavedStreetVertex] = None
      var endEndpointOpt: Option[UnsavedStreetVertex] = None

      val coupleWays: List[List[OSMNode]] = wayUniqueNodes match {
        case head :: second :: tail ⇒ wayUniqueNodes.sliding(2).toList
        case _                      ⇒ Nil
      }

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

          frontEdgeOpt.foreach(frontEdge ⇒ addEdgeToCreatedVertex(startEndpointOpt.get, frontEdge))
          backEdgeOpt.foreach(backEdge ⇒ addEdgeToCreatedVertex(endEndpointOpt.get, backEdge))

          startNode = Some(secondNode.id)
          osmStartNodeOpt = Some(secondNode)

        }
      }
    }
  }

  private def addEdgeToCreatedVertex(streetVertex: UnsavedStreetVertex, streetEdge: StreetEdgeUnsaved): Unit = {

    assert(streetEdge.vertexStartId == streetVertex.id, "The startVertex of the edge must be the Vertex that it belong to.")

    val indexWhere: Int = createdStreetVertex.indexWhere(vertex ⇒ vertex.id == streetVertex.id)

    val foundStreetVertex: UnsavedStreetVertex = createdStreetVertex(indexWhere)

    val updatedStreetVertex: UnsavedStreetVertex = foundStreetVertex.copy(edges = streetEdge :: foundStreetVertex.edges)

    createdStreetVertex.update(indexWhere, updatedStreetVertex)

  }

  private def createEdgesForStreet(startEndpoint: UnsavedStreetVertex, endEndpoint: UnsavedStreetVertex, way: Way, osmStartNode: OSMNode,
    osmEndNode: OSMNode, permissions: StreetTraversalPermission): (Option[StreetEdgeUnsaved], Option[StreetEdgeUnsaved]) = {
    // TODO implementar permissions.allowsNothing (se usa en linea 1006)
    // TODO LineString backGeometry (linea 1010)
    // TODO implementar: "double length = this.getGeometryLengthMeters(geometry);" (linea 1012)

    val (permissionsFront, permissionsBack) = getPairPermissionsForWay(permissions, way)

    val frontOpt =
      if (permissionsFront.allowsAnything())
        Some(createStreetEdgeUnsaved(startEndpoint, endEndpoint, way, permissions))
      else
        None
    val backOpt =
      if (permissionsBack.allowsAnything())
        Some(createStreetEdgeUnsaved(endEndpoint, startEndpoint, way, permissions))
      else
        None

    // TODO revisar el shareData() y el way.isRoundabout() (linea 1028 y 1032)

    (frontOpt, backOpt)
  }

  private def createStreetEdgeUnsaved(startEndpoint: UnsavedStreetVertex, endEndpoint: UnsavedStreetVertex, way: Way,
    permissions: StreetTraversalPermission): StreetEdgeUnsaved = {
    // TODO crear label y name (linea 1046)
    // TODO crear length a partir del recorrido de los coordinate (1053)

    // TODO revisar implementacion de cls (linea 1078)}
    // TODO revisar wheelChairAccesible (linea 1087)
    // TODO revisar slope override (linea 1092)
    // TODO revisar todos los datos extra que hay del way, por ejemplo la "car speed"

    // TODO implementar "permissions: StreetTraversalPermission" para distinguir entre ida y vuelta

    val streetInfo = streetInfoByWayId.getOrElse(way.id, {
      val si = StreetInfo(None, way.name, way.id)
      streetInfoByWayId += (way.id -> si)
      si
    })

    StreetEdgeUnsaved(startEndpoint.id, endEndpoint.id, 10, way.id, streetInfo)
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

  private def createGraphVertex(way: Way, osmStartNode: OSMNode): UnsavedStreetVertex = {
    if (osmStartNode.isMultiLevel) {
      recordLevel(way, osmStartNode)
    } else {
      createdStreetVertex.find(_.id == osmStartNode.id) match {
        case None ⇒
          val vertex: UnsavedStreetVertex = Way.createOSMVertex(way, osmStartNode)
          createdStreetVertex += vertex
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

  private def recordLevel(way: Way, osmStartNode: OSMNode): UnsavedStreetVertex = {
    val level: OSMLevel = osmModule.wayLevels.getOrElse(way, OSMLevel.default)

    val map: Map[OSMLevel, UnsavedStreetVertex] = multiLevelNodes.getOrElse(osmStartNode.id, Map[OSMLevel, UnsavedStreetVertex]())

    map.getOrElse(level, {
      val vertex = Way.createOSMVertex(way, osmStartNode)
      createdStreetVertex += vertex

      val finalMap = map + ((level, vertex))
      multiLevelNodes += (osmStartNode.id -> finalMap)
      vertex
    })
  }
}
