package de.toomuchcoffee.assignment

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers.shouldBe

import java.nio.file.{Path, Paths}

class FunctionalitiesTest extends AnyFunSuiteLike {

  test("printTree should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/tree.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)

    val expectedTree = readFileContent(outputPath)
    val rootNode = Functionalities.buildTree(nodes)
    val printedTree = Functionalities.printTree(rootNode)+"\n"

    val expectedLines = expectedTree.replaceAll("\r\n", "\n").split("\n", -1)
    val actualLines = printedTree.replaceAll("\r\n", "\n").split("\n", -1)

    expectedLines.length shouldBe actualLines.length

    expectedLines.zip(actualLines).zipWithIndex.foreach { case ((e, a), idx) =>
      withClue(s"Line ${idx + 1} differs:\nExpected: '$e'\nActual  : '$a'\n") {
        e shouldBe a
      }
    }
  }

  private def getResourceRootPath: Path =
    val url = getClass.getResource("/") // root of resources folder
    require(url != null, "Resource root path not found")
    Paths.get(url.toURI)

  private def readFileContent(path: String): String =
    val source = scala.io.Source.fromFile(path)
    try source.mkString
    finally source.close()
}
