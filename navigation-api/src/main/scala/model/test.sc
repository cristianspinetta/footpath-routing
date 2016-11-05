import mapdomain.graph.Coordinate
import model.{BusPath, Path, PathDescription, WalkPath}
import searching.{PedestrianIncident, SidewalkIncidentType}

val path = Path(List(Coordinate(19, 20), Coordinate(21.0, -5.5555)), PathDescription(WalkPath, "stop from address", "stop to address"), List(PedestrianIncident(SidewalkIncidentType, None, Some(Coordinate(-9, 11)), Some(Coordinate(-2.4421, 1234)))))
val pathJson = utils.JsonUtil.toJson(path)

val path2 = utils.JsonUtil.fromJson[Path](pathJson)