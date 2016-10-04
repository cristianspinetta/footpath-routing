package mapgenerator.source.osm

import java.net.URL

import base.LazyLoggerSupport
import base.conf.MapTestConfig

trait BaseOSMSpec extends MapTestConfig with LazyLoggerSupport {

  val osmURL: URL = getClass.getResource(configuration.OSM.sourceFilePath)
  val graphJsonURL: URL = getClass.getResource(configuration.OSM.otpFilePath)

}
