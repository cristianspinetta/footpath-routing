package mapgenerator.sidewalk

import mapdomain.sidewalk.{ SidewalkVertex, StreetCrossingEdge }
import scala.collection.concurrent.TrieMap

case class StreetCrossingBuilderManager() {

  val _builders: TrieMap[(SidewalkVertexBuilder, SidewalkVertexBuilder), StreetCrossingBuilder] = TrieMap.empty

  def create(from: SidewalkVertexBuilder, to: SidewalkVertexBuilder): StreetCrossingBuilder = {
    _builders.getOrElseUpdate((from, to), StreetCrossingBuilder(from, to))
  }

  def builders: List[StreetCrossingBuilder] = _builders.values.toList
}

case class StreetCrossingBuilder(from: SidewalkVertexBuilder, to: SidewalkVertexBuilder) {

  def crossKey(fromId: Long, toId: Long): String = {
    val idPart = if (fromId > toId) s"$toId-$fromId" else s"$fromId-$toId"
    s"$idPart-cross"
  }

  def build(implicit idGenerator: SidewalkVertexIDGenerator): (StreetCrossingEdge, SidewalkVertex, SidewalkVertex) = {
    val vertexFrom: SidewalkVertex = from.build
    val vertexTo: SidewalkVertex = to.build
    (StreetCrossingEdge(vertexFrom.id, vertexTo.id, crossKey(vertexFrom.id, vertexTo.id)), vertexFrom, vertexTo)
  }
}