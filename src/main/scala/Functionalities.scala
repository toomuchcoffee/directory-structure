package de.toomuchcoffee.assignment

import model.{Directory, File, Node}

object Functionalities:

  def buildTree(nodes: Seq[Node]): Node = {
    val byParent: Map[Option[Int], Seq[Node]] =
      nodes.groupBy(_.parentId)

    def build(node: Node): Node = node match {
      case dir: Directory =>
        val children = byParent.getOrElse(Some(dir.id), Nil).map(build)
        dir.copy(children = children)
      case _ => node
    }

    val root: Node = byParent.get(None) match {
      case Some(Seq(root)) => root
      case _ => throw new IllegalArgumentException("Tree has none or multiple root(s)")
    }

    build(root)
  }

  def printTree(node: Node, indent: String = ""): String = node match {
    case d: Directory =>
      val childrenStr = d.children.sortBy(_.name).map(printTree(_, indent + " ")).mkString("\n")
      s"${indent}name = ${d.name}, type = Directory, size = ${d.size}\n$childrenStr"
    case f: File =>
      s"${indent}name = ${f.name}, type = File, size = ${f.size}, classification = ${f.classification.value}, checksum = ${f.checksum}"
  }

