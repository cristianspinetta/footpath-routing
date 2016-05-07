package pathgenerator.utils

import pathgenerator.graph.Edge

trait GraphUtils {

  def readableEdges(edges: List[Edge]): String = {
    edges.headOption.map(_.nodeStart).toList ::: edges.map(_.nodeEnd) mkString " -> "
  }
}

object GraphUtils extends GraphUtils
