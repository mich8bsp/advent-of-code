package aoc2021

import scala.annotation.tailrec

object Day20 {

//  def printImage(image: Map[(Int, Int), Char]): Unit = {
//    val minRow = image.keySet.map(_._1).min
//    val minCol = image.keySet.map(_._2).min
//    val normalizedImage = image.map({
//      case ((i, j), c) => (i - minRow, j - minCol) -> c
//    })
//
//    val maxRow = normalizedImage.keySet.map(_._1).max
//    val maxCol = normalizedImage.keySet.map(_._2).max
//    val grid = (0 to maxRow).map(row => {
//      (0 to maxCol).map(col => normalizedImage((row, col))).toArray
//    }).toArray
//
//    println("---------------")
//    println(grid.map(_.mkString("Array(", ", ", ")")).mkString("\n"))
//    println("---------------")
//  }

  def parseInput(isTest: Boolean = false): (String, Map[(Int, Int), Char]) = {
    val lines = readFileLines[String](20, isTest = isTest)
    val key = lines.head

    val grid = lines.tail.tail.map(_.toCharArray).toArray
    val image = (for {
      i <- grid.indices
      j <- grid(i).indices
    } yield {
      (i, j) -> grid(i)(j)
    }).toMap
    (key, image)
  }

  def applyKernel(image: Map[(Int, Int), Char],
                  key: String,
                  iterationNumber: Int): Map[(Int, Int), Char] = {
    val rows: Set[Int] = image.keySet.map(_._1)
    val cols: Set[Int] = image.keySet.map(_._2)
    val rowsRange: Range = rows.min to rows.max
    val colsRange: Range = cols.min to cols.max
    val rowsRangeForKernel: Range = rows.min - 1 to rows.max + 1
    val colsRangeForKernel: Range = cols.min - 1 to cols.max + 1

    val pixelOutOfImageRange: Char = (key.head, key.last) match {
      case ('.', _) => '.' // applying a kernel on 9 '.' gives a '.' so nothing changes when we apply it infinitely
      case ('#', '#') => if(iterationNumber == 0) '.' else '#' // after first iteration 9 '.' turn into 9 '#' and they remain stable after that
      case ('#', '.') => if (iterationNumber % 2 == 0) '.' else '#' // a square of 9 '.' produces a '#' and vice versa so if applied to infinity, it changes each iteration
    }

    (for {
      i <- rowsRangeForKernel
      j <- colsRangeForKernel
    } yield {
      val subImageFlattened: String = (for {
        neighborI <- Seq(i - 1, i, i + 1)
        neighborJ <- Seq(j - 1, j, j + 1)
      } yield {
        if (rowsRange.contains(neighborI) && colsRange.contains(neighborJ)) {
          image.getOrElse((neighborI, neighborJ), '.')
        } else {
          pixelOutOfImageRange
        }
      }).mkString

      val keyIndex: Int = Integer.parseInt(subImageFlattened.replaceAll("\\.", "0").replaceAll("#", "1"), 2)
      val resultPixel: Char = key(keyIndex)
      (i, j) -> resultPixel
    }).toMap
  }

  @tailrec
  def processImage(image: Map[(Int, Int), Char], key: String, iterationsToRun: Int, currentIterationNum: Int = 0): Map[(Int, Int), Char] = {
    //    printImage(image)
    if (iterationsToRun == 0) {
      image
    } else {
      val processed = applyKernel(image, key, currentIterationNum)
      processImage(processed, key, iterationsToRun - 1, currentIterationNum + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val (keyTest, imageTest) = parseInput(isTest = true)

    val processedImageTest = processImage(image = imageTest, key = keyTest, iterationsToRun = 50)
    println(processedImageTest.values.count(_ == '#'))

    val (key, image) = parseInput()

    val processedImage = processImage(image = image, key = key, iterationsToRun = 50)
    println(processedImage.values.count(_ == '#'))
  }
}
