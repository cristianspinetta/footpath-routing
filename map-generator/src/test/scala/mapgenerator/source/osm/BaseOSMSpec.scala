package mapgenerator.source.osm

import java.net.URL

import conf.MapTestConfig

trait BaseOSMSpec extends MapTestConfig {

  val osmURL: URL = getClass.getResource(configuration.OSM.sourceFilePath)
  val graphJsonURL: URL = getClass.getResource(configuration.OSM.otpFilePath)

}
