package de.toomuchcoffee.assignment.model

sealed trait Node:
  def id: Int

  def parentId: Option[Int]

case class Directory(id: Int,
                     parentId: Option[Int],
                     name: String) extends Node

case class File(id: Int,
                parentId: Option[Int],
                name: String,
                size: Int,
                classification: Classification,
                checksum: Int) extends Node

enum Classification(val value: String):
  case Public extends Classification("public")
  case Secret extends Classification("secret")
  case TopSecret extends Classification("top secret")

object Classification:
  def fromString(s: String): Option[Classification] = s.toLowerCase match
    case "public" => Some(Public)
    case "secret" => Some(Secret)
    case "top secret" => Some(TopSecret)
    case _ => None
