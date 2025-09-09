package de.toomuchcoffee.assignment

import model.{Directory, File, Node}

object Functionalities:

  def buildTree(nodes: Seq[Node]): Node = {
    val byParent: Map[Option[Int], Seq[Node]] =
      nodes.groupBy(_.parentId)

    def build(node: Node): Node = node match {
      case dir: Directory =>
        dir.copy(children = byParent.getOrElse(Some(dir.id), Nil).map(build))
      case _ => node
    }

    val root: Node = byParent.get(None) match {
      case Some(Seq(root)) => root
      case _ => throw new IllegalArgumentException("Tree has none or multiple root(s)")
    }

    build(root)
  }

