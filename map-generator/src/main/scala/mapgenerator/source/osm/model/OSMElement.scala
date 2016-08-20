package mapgenerator.source.osm.model

import mapdomain.graph.Coordinate
import org.joda.time.DateTime

trait OSMElement {
  def id: Long
  def tags: Map[String, String]

  private lazy val _highwayOpt: Option[String] = tags.get("highway")

  lazy val isRoutable = tags.contains("highway") ||
    ((tags.get("public_transport").contains("platform") || tags.get("railway").contains("platform")) &&
      !tags.get("usage").contains("tourism"))

  lazy val isRoutableHighway: Boolean = !_highwayOpt.contains("conveyer") &&
    !_highwayOpt.contains("proposed") &&
    !_highwayOpt.contains("construction") &&
    !_highwayOpt.contains("raceway") &&
    !_highwayOpt.contains("unbuilt")

  lazy val isGeneralAccessDenied: Boolean = tags.get("access").contains("no") || tags.get("access").contains("license")

  lazy val isMotorcarExplicitlyAllowed: Boolean = tags.get("motorcar").exists(m ⇒ List("yes", "1", "true", "designated", "official", "permissive", "unknown").contains(m))
  lazy val isBicycleExplicitlyAllowed: Boolean = tags.get("bicycle").exists(m ⇒ List("yes", "1", "true", "designated", "official", "permissive", "unknown").contains(m))
  lazy val isBicycleDismountForced: Boolean = tags.get("bicycle").contains("dismount") || tags.get("cycleway").contains("dismount")
  lazy val isPedestrianExplicitlyAllowed: Boolean = tags.get("foot").exists(m ⇒ List("yes", "1", "true", "designated", "official", "permissive", "unknown").contains(m))
  lazy val isMotorVehicleExplicitlyAllowed: Boolean = tags.get("motor_vehicle").exists(m ⇒ List("yes", "1", "true", "designated", "official", "permissive", "unknown").contains(m))

  lazy val isMotorcarExplicitlyDenied: Boolean = isTagDeniedAccess("motorcar")
  lazy val isMotorVehicleExplicitlyDenied: Boolean = isTagDeniedAccess("motor_vehicle")

  lazy val isBicycleExplicitlyDenied: Boolean = isTagDeniedAccess("bicycle") || tags.get("bicycle").contains("use_sidepath")

  lazy val isPedestrianExplicitlyDenied: Boolean = isTagDeniedAccess("foot")
  lazy val isUnderConstruction: Boolean = tags.get("highway").contains("construction") || tags.get("cycleway").contains("construction")

  lazy val isParkAndRide: Boolean = tags.get("amenity").contains("parking") &&
    (tags.get("parking").contains("park_and_ride") || tags.get("park_ride").exists(_ != "no"))

  lazy val isBikeParking: Boolean = {
    val access: Option[String] = tags.get("access")
    tags.get("amenity").contains("bicycle_parking") && !access.contains("private") && !access.contains("no")
  }

  def isTagDeniedAccess(tagName: String): Boolean = {
    val valueTag: Option[String] = tags.get(tagName)
    valueTag.contains("no") || valueTag.contains("license")
  }

  /**
   * Determines whether this OSM way is considered routable. The majority of routable ways are
   * those with a highway= tag (which includes everything from motorways to hiking trails).
   * Anything with a public_transport=platform or railway=platform tag is also considered routable
   * even if it doesn't have a highway tag. Platforms are however filtered out if they are marked
   * usage=tourism. This prevents miniature tourist railways like the one in Portland's Zoo from
   * receiving a better score and pulling search endpoints away from real transit stops.
   */
  lazy val isOsmEntityRoutable: Boolean = {
    tags.contains("highway") ||
      (
        (
          tags.get("public_transport").contains("platform") ||
          tags.get("railway").contains("platform"))
          &&
          !tags.get("usage").contains("tourism"))
  }

  /**
   * Determine whether any mode can or should ever traverse the given way. If not, we leave the
   * way out of the OTP graph. Potentially routable ways are those that have the tags : highway=*
   * public_transport=platform railway=platform
   *
   * But not conveyers, proposed highways/roads or those still under construction, and raceways
   * (as well as ways where all access is specifically forbidden to the public).
   * http://wiki.openstreetmap.org/wiki/Tag:highway%3Dproposed
   */
  lazy val isRoutableWay: Boolean = {

    // TODO se puede reducir, pero estoy quemado para hacerlo ahora. ¿podria ser asi: "return isOsmEntityRoutable(osmElem) && osmElem. isRoutable && ..."?
    if (!isOsmEntityRoutable) false
    else {
      this.isRoutable &&
        this.isRoutableHighway &&
        (!this.isGeneralAccessDenied || (this.isMotorcarExplicitlyAllowed || this.isBicycleExplicitlyAllowed || this.isPedestrianExplicitlyAllowed || this.isMotorVehicleExplicitlyAllowed))
    }
  }
}

object OSMElement {

  def isTagTrue(tags: Map[String, String], tagName: String): Boolean =
    tags.get(tagName).exists(m ⇒ m == "yes" || m == "1" || m == "true")

  def isTagFalse(tags: Map[String, String], tagName: String): Boolean =
    tags.get(tagName).exists(m ⇒ m == "no" || m == "0" || m == "false")
}

case class Member(`type`: String, ref: Long, role: String) // FIXME use Role and Type as Enum or object

case class Bounds(minlat: Double, minlon: Double, maxlat: Double, maxlon: Double)
