package de.toomuchcoffee.assignment.model

sealed trait Node:
  def id: Int

  def parentId: Option[Int]

  def name: String

  def size: Int

  def toDisplayString(indent: String = ""): String

case class Directory(id: Int,
                     parentId: Option[Int],
                     name: String,
                     children: Seq[Node] = Seq.empty) extends Node {
  override def size: Int = children.map(_.size).sum

  override def toDisplayString(indent: String = ""): String =
    val childrenStr = children.sortBy(_.name).map(_.toDisplayString(indent + " ")).mkString("\n")
    s"${indent}name = $name, type = Directory, size = $size\n$childrenStr"
}

case class File(id: Int,
                parentId: Option[Int],
                name: String,
                size: Int,
                classification: Classification,
                checksum: Int) extends Node {

  override def toDisplayString(indent: String = ""): String =
    s"${indent}name = $name, type = File, size = $size, classification = ${classification.value}, checksum = $checksum"
}



enum Classification(val value: String):
  case Public extends Classification("Public")
  case Secret extends Classification("Secret")
  case TopSecret extends Classification("Top secret")

object Classification:
  def fromString(s: String): Option[Classification] = s.toLowerCase match
    case "public" => Some(Public)
    case "secret" => Some(Secret)
    case "top secret" => Some(TopSecret)
    case _ => None
