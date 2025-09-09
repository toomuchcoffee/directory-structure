package de.toomuchcoffee.assignment

import model.Classification.{Secret, TopSecret}

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers.shouldBe

import java.nio.file.{Path, Paths}

class FunctionalitiesTest extends AnyFunSuiteLike {

  test("buildTree should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/tree.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)
    val tree = Functionalities.buildTree(nodes)

    val actual = tree.toDisplayString()
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    expected shouldBe actual
  }

  test("filterBy 'top-secret' should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/top-secret.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)
    val result = Functionalities.filterBy(nodes, TopSecret)

    val actual = result.map(_.toDisplayString()).mkString("\n")
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    expected shouldBe actual
  }

  test("filterBy 'secret' should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/secret.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)
    val result = Functionalities.filterBy(nodes, Secret)

    val actual = result.map(_.toDisplayString()).mkString("\n")
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    expected shouldBe actual
  }

  test("filterBy 'secret' or 'top-secret' should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/secret-or-top-secret.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)
    val result = Functionalities.filterBy(nodes, Secret, TopSecret)

    val actual = result.map(_.toDisplayString()).mkString("\n")
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    expected shouldBe actual
  }

  private def getResourceRootPath: Path =
    val url = getClass.getResource("/")
    require(url != null, "Resource root path not found")
    Paths.get(url.toURI)

  private def readFileContent(path: String): String =
    val source = scala.io.Source.fromFile(path)
    try source.mkString
    finally source.close()
}
