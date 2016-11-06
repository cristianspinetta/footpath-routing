package utils

import model.{ Path, PathTypeSerializer }
import org.json4s.DefaultFormats
import searching.IncidentTypeSerializer
import org.json4s.jackson.Serialization.{ read, write }

object JsonUtils {

  implicit val formats = DefaultFormats + PathTypeSerializer + IncidentTypeSerializer

  def toJson(value: Path): String = {
    write[Path](value)
  }

  def fromJson(json: String): Path = {
    read[Path](json)
  }

}
