package aoc2025

import scala.annotation.tailrec
import scala.collection.mutable

object Day4 {

  private def countAccessibleRolls(grid: Array[Array[Char]]): Int = {
    val accessibleRolls = for {
      row <- grid.indices
      col <- grid(row).indices if grid(row)(col) == '@'
    } yield {
      val neighbors = for {
        i <- (-1 to 1) if row + i >= 0 && row + i < grid.length
        j <- (-1 to 1) if !(j == 0 && i == 0) && col + j >= 0 && col + j < grid(row).length
        if grid(row+i)(col+j) == '@'
      } yield {
        1
      }

      if (neighbors.sum < 4) {
        1
      } else {
        0
      }
    }

    accessibleRolls.sum
  }

  @tailrec
  private def countAccessibleRolls2(grid: Array[Array[Char]], removedRolls: mutable.Set[(Int, Int)] = mutable.Set.empty[(Int, Int)]): Int = {
    val accessibleRolls = (for {
      row <- grid.indices
      col <- grid(row).indices if grid(row)(col) == '@' && !removedRolls.contains((row, col))
    } yield {
      val neighbors = for {
        i <- (-1 to 1) if row + i >= 0 && row + i < grid.length
        j <- (-1 to 1) if !(j == 0 && i == 0) && col + j >= 0 && col + j < grid(row).length
        if grid(row+i)(col+j) == '@' && !removedRolls.contains((row+i, col+j))
      } yield {
        1
      }

      if (neighbors.sum < 4) {
        Some((row, col))
      } else {
        None
      }
    }).flatten

    if (accessibleRolls.nonEmpty) {
      removedRolls.addAll(accessibleRolls)
      countAccessibleRolls2(grid, removedRolls)
    } else {
      removedRolls.size
    }
  }


  def main(args: Array[String]): Unit = {
    val grid = readCharGrid(4)
    println(countAccessibleRolls(grid))
    println(countAccessibleRolls2(grid))
  }
}
