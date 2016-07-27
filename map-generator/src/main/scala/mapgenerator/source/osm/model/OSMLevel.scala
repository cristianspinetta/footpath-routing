package mapgenerator.source.osm.model

import scala.util.Try

case class OSMLevel(floorNumber: Int, altitudeMeters: Double, shortName: String, longName: String,
  source: Source, reliable: Boolean)

object OSMLevel {

  val metersPerFloor = 3

  val default = new OSMLevel(0, 0.0, "default level", "default level", NONE, true)

  /**
   * makes an OSMLevel from one of the semicolon-separated fields in an OSM
   * level map relation's levels= tag.
   */
  def fromString(spec: String, source: Source, incrementNonNegative: Boolean): OSMLevel = {

    /*  extract any altitude information after the @ character */
    val lastIndexAt: Int = spec.lastIndexOf('@')

    val altitudeOpt: Option[Double] = if (lastIndexAt != -1)
      Try(spec.substring(lastIndexAt + 1).toDouble).toOption
    else
      None

    val shortName: String = firstExtract
      .andThen(extractEqualPartForShortName)
      .andThen(extractNumberFromAnyName)
      .andThen((s: String) ⇒ processName(source, incrementNonNegative, s))(spec)

    val longName: String = firstExtract
      .andThen(extractEqualPartForLongName)
      .andThen(extractNumberFromAnyName)
      .andThen((s: String) ⇒ processName(source, incrementNonNegative, s))(spec)

    val (floorNumber, reliable): (Int, Boolean) = Try(shortName.toInt)
      .map(floorNumber ⇒
        (processFloorNumber(source, incrementNonNegative, floorNumber), true))
      .recoverWith {
        case (_: Throwable) ⇒
          Try(longName.toInt)
            .map(floorNumber ⇒
              (processFloorNumber(source, incrementNonNegative, floorNumber), true))
      }
      .getOrElse {
        val floorNumber: Int = altitudeOpt
          .map(altitude ⇒ (altitude / metersPerFloor).toInt)
          .getOrElse(0)
        (floorNumber, false)
      }

    val altitude = altitudeOpt.getOrElse(0.toDouble)

    new OSMLevel(floorNumber, altitude, shortName, longName, source, reliable)
  }

  private def processFloorNumber(source: Source, incrementNonNegative: Boolean, floorNumber: Int): Int = {
    if (incrementNonNegative && source == LEVEL_MAP && floorNumber >= 1)
      floorNumber - 1 // level maps are localized, floor numbers are 0-based
    else floorNumber
  }

  private def processName(source: Source, incrementNonNegative: Boolean, name: String): String = Try(name.toInt) map ((floorNumber: Int) ⇒ {
    if (incrementNonNegative && source != LEVEL_MAP && floorNumber >= 0) // level and layer tags are 0-based
      (floorNumber + 1).toString
    else
      floorNumber.toString
  }) getOrElse name

  private val firstExtract: String ⇒ String = (s: String) ⇒ {
    val index: Integer = s.lastIndexOf('@')
    if (index != -1) s.substring(0, index)
    else s
  }

  private val extractEqualPartForShortName: String ⇒ String = (spec: String) ⇒ {
    val indexEquals: Integer = spec.indexOf('=')
    if (indexEquals >= 1)
      spec.substring(0, indexEquals)
    else
      // set them both the same, the trailing @altitude has already been discarded
      spec
  }

  private val extractEqualPartForLongName: String ⇒ String = (spec: String) ⇒ {
    val indexEquals: Integer = spec.indexOf('=')
    if (indexEquals >= 1)
      spec.substring(indexEquals + 1)
    else
      // set them both the same, the trailing @altitude has already been discarded
      spec
  }

  private val extractNumberFromAnyName: String ⇒ String = (s: String) ⇒ {
    if (s.startsWith("+")) s.substring(1)
    else s
  }
}

trait Source
case object LEVEL_MAP extends Source
case object LEVEL_TAG extends Source
case object LAYER_TAG extends Source
case object ALTITUDE extends Source
case object NONE extends Source
