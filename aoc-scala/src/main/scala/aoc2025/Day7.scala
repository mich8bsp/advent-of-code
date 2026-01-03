package aoc2025

import scala.collection.mutable

object Day7 {

  def solve(grid: Array[Array[Char]]): Int = {
    val beamPositions = mutable.Set.empty[(Int, Int)]
    beamPositions.add((0, grid.head.indexOf('S')))

    var splits: Int = 0
    grid.indices.tail.foreach { _ =>
      val currentPositions = beamPositions.toSet
      beamPositions.clear()
      currentPositions.foreach {
        case (i, j) =>
          if (grid(i+1)(j) == '.') {
            beamPositions.add((i+1, j))
          } else {
            splits+=1
            if (j >= 1) {
              beamPositions.add((i+1, j-1))
            }
            if (j < grid(i+1).length-1) {
              beamPositions.add((i+1, j+1))
            }
          }
      }
    }
    splits
  }

  def solve2(grid: Array[Array[Char]]): Long = {
    val cache = mutable.Map.empty[(Int, Int), Long]

    def iterate(currentRow: Int, currentCol: Int): Long = {
      if (currentRow == grid.length-1) {
        1L
      } else {
        cache.getOrElseUpdate((currentRow+1, currentCol), {
          if (grid(currentRow+1)(currentCol) == '.') {
            iterate(currentRow + 1, currentCol)
          } else {
            val splitLeft = if (currentCol > 0) {
              iterate(currentRow + 1, currentCol - 1)
            } else {
              0L
            }
            val splitRight = if (currentCol < grid(currentRow+1).length - 1) {
              iterate(currentRow + 1, currentCol + 1)
            } else {
              0L
            }

            splitLeft + splitRight
          }
        })
      }
    }

    iterate(0, grid.head.indexOf('S'))
  }

  def main(args: Array[String]): Unit = {
    val grid = readCharGrid(7)
    println(solve(grid))
    println(solve2(grid))
  }
}
