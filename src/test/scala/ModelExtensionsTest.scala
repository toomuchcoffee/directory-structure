package de.toomuchcoffee.assignment

import ModelExtensions.{buildTree, filterBy, findBy, sizeBy}
import Classification.{Public, Secret, TopSecret}

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers.shouldBe

import java.nio.file.{Path, Paths}

class ModelExtensionsTest extends AnyFunSuiteLike {

  def withData(testCode: (Node, Path) => Any): Unit = {
    val resourceRoot =
      val url = getClass.getResource("/")
      require(url != null, "Resource root path not found")
      Paths.get(url.toURI)
    val inputPath = resourceRoot.resolve("input/directory-structure.csv").toString
    val (errors, nodes) = FileProcessor.processFile(inputPath)
    val tree = nodes.buildTree()
    assert(errors.isEmpty)
    testCode(tree, resourceRoot)
  }

  test("Should print tree correctly") {
    withData { (tree, resourceRoot) =>
      val outputPath = resourceRoot.resolve("output/tree.txt").toString

      val actual = tree.toDisplayString()
      val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

      actual shouldBe expected
    }
  }

  test("filterBy 'top-secret' should create correct result") {
    withData { (tree, resourceRoot) =>
      val outputPath = resourceRoot.resolve("output/top-secret.txt").toString

      val result = tree.filterBy(TopSecret)

      val actual = result.map(_.toDisplayString()).mkString("\n")
      val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

      actual shouldBe expected
    }
  }

  test("filterBy 'secret' should create correct result") {
    withData { (tree, resourceRoot) =>
      val outputPath = resourceRoot.resolve("output/secret.txt").toString

      val result = tree.filterBy(Secret)

      val actual = result.map(_.toDisplayString()).mkString("\n")
      val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

      actual shouldBe expected
    }
  }

  test("filterBy 'secret' or 'top-secret' should create correct result") {
    withData { (tree, resourceRoot) =>
      val outputPath = resourceRoot.resolve("output/secret-or-top-secret.txt").toString

      val result = tree.filterBy(Secret, TopSecret)

      val actual = result.map(_.toDisplayString()).mkString("\n")
      val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

      actual shouldBe expected
    }
  }

  test("sizeBy 'public' should create correct result") {
    withData { (tree, _) =>
      val result = tree.sizeBy(Public)

      result shouldBe 120L
    }
  }

  test("filterBy 'non-public' in 'folder11' should create correct result") {
    withData { (tree, resourceRoot) =>
      val outputPath = resourceRoot.resolve("output/non-public-folder11.txt").toString

      val folder11 = tree.findBy("folder11")
      assert(folder11.isDefined)
      val result = folder11.get.filterBy(Secret, TopSecret)

      val actual = result.map(_.toDisplayString()).mkString("\n")
      val expected = readFileContent(outputPath).replaceAll("\r\n", "\n").trim

      actual shouldBe expected
    }
  }


  private def readFileContent(path: String): String =
    val source = scala.io.Source.fromFile(path)
    try source.mkString
    finally source.close()
}
