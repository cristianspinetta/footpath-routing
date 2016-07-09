package pathgenerator.utils

import pathgenerator.graph.Edge

trait GraphUtils {

  def readableEdges(edges: List[Edge]): String =
    edgesToIds(edges) mkString " -> "

  def edgesToIds(edges: List[Edge]): List[Long] =
    edges.headOption.map(_.vertexStart).toList ::: edges.map(_.vertexEnd)

}

object GraphUtils extends GraphUtils
