package mapgenerator.source.osm.model

import org.joda.time.DateTime
import pathgenerator.graph.Coordinate

case class OSMNode(lon: Double, lat: Double, uid: Long, user: String, timestamp: DateTime, changeset: Long,
    version: Int, visible: Boolean, id: Long, tags: Map[String, String]) extends OSMElement {

  lazy val coordinate: Coordinate = Coordinate(lat, lon)

  /**
   * Is this a public transport stop that can be linked to a transit stop vertex on the graph later on.
   *
   * @return
   */
  lazy val isStop: Boolean = {
    tags.get("highway").contains("bus_stop") ||
      tags.get("railway").contains("tram_stop") ||
      tags.get("railway").contains("station") ||
      tags.get("railway").contains("halt") ||
      tags.get("amenity").contains("bus_station")
  }

  lazy val isBollard: Boolean = tags.get("barrier").contains("bollard")

  lazy val isMultiLevel: Boolean = tags.get("highway").contains("elevator")

  lazy val isBikeRental: Boolean = tags.get("amenity").contains("bicycle_rental")
}
