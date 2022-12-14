package aoc2022

import scala.annotation.tailrec

object Day14 {

  private def initWithRockFormation(rockFormation: List[(Int, Int)])
                                   (implicit grid: Array[Array[Char]]): Unit = {
    rockFormation.sliding(2).foreach { window =>
      val prev = window.head
      val curr = window(1)
      for {
        row <- math.min(prev._1, curr._1) to math.max(prev._1, curr._1)
        col <- math.min(prev._2, curr._2) to math.max(prev._2, curr._2)
      } yield {
        grid(row)(col) = '#'
      }
    }
  }

  private def isInGrid(pos: (Int, Int))
                      (implicit grid: Array[Array[Char]]): Boolean = {
    val (row, col) = pos
    row >= 0 && col >= 0 && row < grid.length && col < grid(row).length
  }

  private def isEmpty(pos: (Int, Int))
                     (implicit grid: Array[Array[Char]]): Boolean = {
    val (row, col) = pos
    grid(row)(col) == '.'
  }

  @tailrec
  private def simulateGrain(pos: (Int, Int))
                           (implicit grid: Array[Array[Char]]): Option[(Int, Int)] = {
    val (row, col) = pos
    if (!isInGrid((row + 1, col))) {
      None
    } else if (isEmpty((row + 1, col))) {
      simulateGrain(row + 1, col)
    } else if (!isInGrid((row + 1, col - 1))) {
      None
    } else if (isEmpty ((row + 1, col - 1))) {
      simulateGrain(row + 1, col - 1)
    } else if (!isInGrid((row + 1, col + 1))) {
      None
    } else if (isEmpty((row + 1, col + 1))) {
      simulateGrain(row + 1, col + 1)
    } else {
      Some(pos)
    }
  }

  @tailrec
  private def simulateSand(simulated: Int = 0)
                          (implicit grid: Array[Array[Char]]): Int = {
    val startPos = (0, 500)
    if (!isEmpty(startPos)) {
      simulated
    } else {
      val grainRestPos = simulateGrain(startPos)
      grainRestPos match {
        case None => simulated
        case Some((restRow, restCol)) =>
          grid(restRow)(restCol) = 'o'
          simulateSand(simulated + 1)
      }
    }
  }

  private def printGrid(grid: Array[Array[Char]]): Unit = {
    println(grid.map(_.mkString("")).mkString("\n"))
  }

  def solveA(rockFormations: List[List[(Int, Int)]]): Int = {
    val maxRow = rockFormations.flatten.map(_._1).max
    val maxCol = rockFormations.flatten.map(_._2).max
    implicit val grid: Array[Array[Char]] = Array.fill(maxRow + 1, maxCol + 1) { '.' }
    rockFormations.foreach(initWithRockFormation)
//    printGrid(grid)
    val res = simulateSand()
//    printGrid(grid)
    res
  }

  def solveB(rockFormations: List[List[(Int, Int)]]): Int = {
    val rows = rockFormations.flatten.map(_._1)
    val cols = rockFormations.flatten.map(_._2)
    val maxRow = rows.max + 2
    val maxCol = cols.max + maxRow
    implicit val grid: Array[Array[Char]] = Array.fill(maxRow + 1, maxCol + 1) {
      '.'
    }
    (rockFormations :+ List((maxRow, 0), (maxRow, maxCol))).foreach(initWithRockFormation)
//    printGrid(grid)
    val res = simulateSand()
//    printGrid(grid)
    res
  }

  def main(args: Array[String]): Unit = {
    val rockFormations: List[List[(Int, Int)]] = readFileLines(14) { line => {
      line.split("->")
        .map(_.trim)
        .map(coord => {
          val Array(col, row) = coord.split(",")
          (row.toInt, col.toInt)
        })
        .toList
    }}

    println(solveA(rockFormations))
    println(solveB(rockFormations))
  }
}
