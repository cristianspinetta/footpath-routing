package mapgenerator.source.osm

import java.net.URL

trait BaseOSMSpec {

  val osmURL: URL = getClass.getResource("/map.osm")
  val graphJsonURL: URL = getClass.getResource("/graph.json")

}
