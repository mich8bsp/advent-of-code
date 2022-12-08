package aoc2022

import scala.annotation.tailrec
import scala.collection.mutable


object Day8 {
  object Direction extends Enumeration {
    val LEFT, RIGHT, UP, DOWN = Value
  }

  def nextPosByDirection(i: Int, j: Int, direction: Direction.Value): (Int, Int) = direction match {
    case Direction.UP => (i - 1, j)
    case Direction.DOWN => (i + 1, j)
    case Direction.LEFT => (i, j - 1)
    case Direction.RIGHT => (i, j + 1)
  }

  def calculateHighestTreeFromIdx(trees: Array[Array[Int]]): Array[Array[mutable.Map[Direction.Value, Int]]] = {
    val cache: mutable.Map[(Int, Int, Direction.Value), Int] = mutable.Map.empty
    val rows: Int = trees.length
    val cols: Int = trees.head.length

    val output: Array[Array[mutable.Map[Direction.Value, Int]]] = Array.fill(rows, cols) {
      mutable.Map.empty
    }

    def getHighestTreeInDirection(i: Int, j: Int, direction: Direction.Value): Int = cache.getOrElseUpdate((i, j, direction), {
      (i, j, direction) match {
        case (0, _, Direction.UP) | (_, 0, Direction.LEFT) => -1
        case (row, _, Direction.DOWN) if row == rows - 1 => -1
        case (_, col, Direction.RIGHT) if col == cols - 1 => -1
        case _ =>
          val (nextI, nextJ) = nextPosByDirection(i, j, direction)
          math.max(trees(nextI)(nextJ), getHighestTreeInDirection(nextI, nextJ, direction))
      }
    })

    for {
      i <- 0 until rows
      j <- 0 until cols
      direction <- Direction.values
    } yield {
      output(i)(j).put(direction, getHighestTreeInDirection(i, j, direction))
    }

    output
  }

  def solveA(trees: Array[Array[Int]], highestTreesMap: Array[Array[mutable.Map[Direction.Value, Int]]]): Int = {
    val rows: Int = trees.length
    val cols: Int = trees.head.length

    (for {
      i <- 0 until rows
      j <- 0 until cols
    } yield {
      val treeHeight = trees(i)(j)
      val highestTreesInAllDirections = highestTreesMap(i)(j)
      if (treeHeight > highestTreesInAllDirections.values.min) {
        1
      } else {
        0
      }
    }).sum
  }

  def solveB(trees: Array[Array[Int]]): Int = {
    val rows: Int = trees.length
    val cols: Int = trees.head.length

    @tailrec
    def getViewingDistance(i: Int, j: Int, direction: Day8.Direction.Value, maxHeight: Int, distance: Int = 0): Int = {
      (i, j, direction) match {
        case (0, _, Direction.UP) | (_, 0, Direction.LEFT) => distance
        case (row, _, Direction.DOWN) if row == rows - 1 => distance
        case (_, col, Direction.RIGHT) if col == cols - 1 => distance
        case _ =>
          val (nextI, nextJ) = nextPosByDirection(i, j, direction)
          if (trees(nextI)(nextJ) < maxHeight) {
            getViewingDistance(nextI, nextJ, direction, maxHeight, distance + 1)
          } else {
            distance + 1
          }
      }
    }

    val scenicScores = for {
      i <- 0 until rows
      j <- 0 until cols
    } yield {
      Direction.values.map(direction => getViewingDistance(i, j, direction, trees(i)(j))).product
    }

    scenicScores.max
  }

  def main(args: Array[String]): Unit = {
    val grid = readDigitsGrid(8)
    val highestTreesMap = calculateHighestTreeFromIdx(grid)
    println(solveA(grid, highestTreesMap))
    println(solveB(grid))
  }
}
