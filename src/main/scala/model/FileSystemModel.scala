package de.toomuchcoffee.assignment.model

sealed trait Node:
  def id: Int

  def parentId: Option[Int]

  def name: String

  def size: Int

case class Directory(id: Int,
                     parentId: Option[Int],
                     name: String,
                     children: Seq[Node] = Seq.empty) extends Node {
  def size: Int = children.map(_.size).sum
}

case class File(id: Int,
                parentId: Option[Int],
                name: String,
                size: Int,
                classification: Classification,
                checksum: Int) extends Node

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
