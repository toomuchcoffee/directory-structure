package de.toomuchcoffee.assignment

import ModelExtensions.{buildTree, filterBy, sizeBy}
import Classification.{Public, Secret, TopSecret}

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers.shouldBe

import java.nio.file.{Path, Paths}

class ModelExtensionsTest extends AnyFunSuiteLike {

  test("buildTree should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/tree.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)

    val tree = nodes.buildTree()

    val actual = tree.toDisplayString()
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    actual shouldBe expected
  }

  test("filterBy 'top-secret' should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/top-secret.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)

    val result = nodes.filterBy(TopSecret)

    val actual = result.map(_.toDisplayString()).mkString("\n")
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    actual shouldBe expected
  }

  test("filterBy 'secret' should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/secret.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)

    val result = nodes.filterBy(Secret)

    val actual = result.map(_.toDisplayString()).mkString("\n")
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    actual shouldBe expected
  }

  test("filterBy 'secret' or 'top-secret' should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val outputPath = resourceRoot.resolve("output/secret-or-top-secret.txt").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)

    val result = nodes.filterBy(Secret, TopSecret)

    val actual = result.map(_.toDisplayString()).mkString("\n")
    val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

    actual shouldBe expected
  }

  test("sizeBy 'public' should create correct result") {
    val resourceRoot = getResourceRootPath
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString

    val (errors, nodes) = FileProcessor.processFile(inputPath)
    assert(errors.isEmpty)

    val result = nodes.sizeBy(Public)

    result shouldBe 120L
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
