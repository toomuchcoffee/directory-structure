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

  extension (node: Node)
    def filterBy(classifications: Classification*): Seq[Node] = {
      val allowed = classifications.toSet

      def recurse(node: Node): Seq[Node] = {
        node match {
          case file: File if allowed.contains(file.classification) => Seq(file)
          case directory: Directory => directory.children.flatMap(child => recurse(child))
          case _ => Seq.empty
        }
      }

      recurse(node).sortBy(_.name)
    }

    def sizeBy(classifications: Classification*): Long =
      filterBy(classifications: _*).map(_.size).sum


    def findBy(name: String): Option[Node] = node match {
      case n: Node if n.name == name => Some(n)
      case d: Directory =>
        d.children.flatMap(child => child.findBy(name).toSeq)
          .headOption
      case _ => None
    }
