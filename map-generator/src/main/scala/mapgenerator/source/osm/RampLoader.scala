package mapgenerator.source.osm

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.Ramp

import scala.io.Source

trait RampLoader {
  def loadRamps: Vector[Ramp]
}

class RampLoaderByCSV[T <: RampLoaderByType](sources: Seq[(Source, T)]) extends RampLoader {

  lazy val loadRamps: Vector[Ramp] =
    sources.flatMap {
      case (source, loader) ⇒
        source.getLines()
          .drop(1)
          .map(loader.create)
    }.toVector
}

trait RampLoaderByType {
  def create(line: String): Ramp
}

object RampLoader2014 extends RampLoaderByType {

  def create(line: String): Ramp = {
    val chucks = line.split(";")
    val number =
      if (chucks(4).nonEmpty)
        Some(chucks(4).toInt)
      else None
    Ramp(wkt2Coordinate(chucks(0)),
      chucks(2),
      chucks(3),
      number,
      chucks(5))
  }

  private def wkt2Coordinate(raw: String): Coordinate = {
    val chucks = raw.split(" ")
    val lat = chucks(2).takeWhile(_ != ')').toDouble
    val lng = chucks(1).drop(1).toDouble
    Coordinate(lat, lng)
  }
}

object RampLoader2011 extends RampLoaderByType {

  def create(line: String): Ramp = {
    val chucks = line.split(";")
    Ramp(Coordinate(chucks(1).toDouble, chucks(0).toDouble),
      chucks(2),
      chucks(2),
      None,
      chucks(2))
  }
}

object RampLoaderByCSV {
  def apply[T <: RampLoaderByType](sources: Seq[(String, T)]): RampLoader = new RampLoaderByCSV(sources.map(srcs ⇒ (Source.fromFile(srcs._1), srcs._2)))
}
