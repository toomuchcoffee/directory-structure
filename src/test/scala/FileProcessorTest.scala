package de.toomuchcoffee.assignment

import model.Classification.{Public, Secret, TopSecret}
import model.{Directory, File}

import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers.shouldBe

import java.nio.file.Paths



class FileProcessorTest extends AnyFunSuiteLike {
  test("processFile should parse all lines into model") {
    val resourceUrl = getClass.getResource("/input/directory-structure.csv")
    assert(resourceUrl != null, "Test file not found!")

    val path = Paths.get(resourceUrl.toURI).toString

    val (errors, nodes) = FileProcessor.processFile(path)

    errors.length shouldBe 0

    nodes.length shouldBe 11

    val expectedNodes = Seq(
      File(1, Some(3), "file1", 10, Secret, 42),
      Directory(2, None, "folder2"),
      Directory(3, Some(11), "folder3"),
      File(4, Some(2), "file4", 40, Secret, 42),
      File(5, Some(3), "file5", 50, Public, 42),
      File(6, Some(3), "file6", 60, Secret, 42),
      File(7, Some(3), "file7", 70, Public, 42),
      File(8, Some(10), "file8", 80, Secret, 42),
      File(9, Some(10), "file9", 90, TopSecret, 42),
      Directory(10, Some(11), "folder10"),
      Directory(11, Some(2), "folder11")
    )

    nodes shouldBe expectedNodes
  }
}