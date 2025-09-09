package de.toomuchcoffee.assignment

object ModelExtensions:
  extension (nodes: Seq[Node])
    def buildTree(): Node = {
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
  
    def filterBy(classifications: Classification*): Seq[Node] = {
      nodes
        .collect { case f: File if classifications.toSet.contains(f.classification) => f }
        .sortBy(_.name)
    }
  
    def sizeBy(classifications: Classification*): Long =
      filterBy(classifications: _*).map(_.size).sum
  
