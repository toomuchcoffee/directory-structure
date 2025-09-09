package de.toomuchcoffee.assignment

import scala.io.Source

object FileProcessor:

  def processFile(path: String): (Seq[String], Seq[Node]) =
    val source = Source.fromFile(path)
    try
      val lines = source.getLines()
        .map(_.trim)
        .filterNot(line => line.isEmpty || line.startsWith("#"))
        .toSeq

      val results = lines.map(processLine)
      val (errors, nodes) = results.partitionMap(identity)
      (errors, nodes)
    finally
      source.close()

  private def processLine(line: String): Either[String, Node] = {
    val elems = line.split(";").map(_.trim).toList
    elems match
      case idStr :: parentIdStr :: name :: nodeType :: rest =>
        for
          id <- parseId(idStr)
          parentId <- parseParentId(parentIdStr)
          node <- nodeType match
            case "directory" if rest.forall(_.isEmpty) =>
              Right(Directory(id, parentId, name))
            case "file" =>
              rest match
                case sizeStr :: classificationStr :: checksumStr :: Nil =>
                  for
                    size <- sizeStr.toIntOption.toRight(s"Invalid integer for size: '$sizeStr'")
                    classification <- Classification.fromString(classificationStr).toRight(s"Invalid classification: '$classificationStr'")
                    checksum <- checksumStr.toIntOption.toRight(s"Invalid integer for checksum: '$checksumStr'")
                  yield File(id, parentId, name, size, classification, checksum)
                case _ =>
                  Left(s"Malformed data for file node: expected 3 fields but got ${rest.length}")
            case other => Left(s"Invalid node type or malformed data: $other")
        yield node
      case _ => Left(s"Unexpected number of fields in line: $line")
  }

  private def parseParentId(parentIdStr: String) = {
      if parentIdStr.isEmpty then Right(None)
      else parentIdStr.toIntOption
        .map(Some(_))
        .toRight(s"Invalid optional integer for parentId: '$parentIdStr'")
    }

    private def parseId(idStr: String) = {
      idStr.toIntOption
        .toRight(s"Invalid integer for id: '$idStr'")
    }


    @main def run(args: String*): Unit =
      if args.isEmpty then
        println("Please provide a file path.")
      else
        val (errors, nodes) = processFile(args(0))
        errors.foreach(println)
        nodes.foreach(println)
