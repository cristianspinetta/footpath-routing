package pathgenerator.utils

import pathgenerator.graph.Edge

trait GraphUtils {

  def printReadableEdges(edges: List[Edge]): String = {
    edges.headOption.map(_.vertexStart).toList ::: edges.map(_.vertexEnd) mkString " -> "
  }
}

object GraphUtils extends GraphUtils
