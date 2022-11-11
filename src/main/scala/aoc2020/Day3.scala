package aoc2020

import scala.annotation.tailrec

object Day3 {

  def solveA(grid: Array[Array[Char]]): Long = {
    solve(grid, (3, 1))
  }

  def solveB(grid: Array[Array[Char]]): Long = {
    Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map(solve(grid, _))
      .product
  }

  def solve(grid: Array[Array[Char]], strategy: (Int, Int)): Long = {
    val numOfCols = grid.head.length
    val numOfRows = grid.length
    val (dx, dy) = strategy

    @tailrec
    def countTrees(pos: (Int, Int), treesCount: Long): Long = pos match {
      case (_, y) if y >= numOfRows => treesCount
      case (x, y) => grid(y)(x % numOfCols) match {
        case '#' => countTrees((x % numOfCols + dx, y + dy), treesCount + 1)
        case '.' => countTrees((x % numOfCols + dx, y + dy), treesCount)
      }
    }

    countTrees((0, 0), 0L)
  }

  def main(args: Array[String]): Unit = {
    val grid = readCharGrid(3)
    println(solveA(grid))
    println(solveB(grid))
  }
}
