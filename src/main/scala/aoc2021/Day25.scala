package aoc2021

import scala.io.Source

object Day25 {

  def getEastHerdMovements(grid: Array[Array[Char]]): Seq[((Int, Int), (Int, Int))] = {
    val cols = grid.head.length
    (for {
      i <- grid.indices
      j <- grid(i).indices
    } yield {
      if (grid(i)(j) == '>') {
        val (destI, destJ) = (i, (j + 1) % cols)
        if (grid(destI)(destJ) == '.') {
          Some((i, j) -> (destI, destJ))
        } else {
          None
        }
      } else {
        None
      }
    }).flatten
  }

  def getSouthHerdMovements(grid: Array[Array[Char]]): Seq[((Int, Int), (Int, Int))] = {
    val rows = grid.length
    (for {
      i <- grid.indices
      j <- grid(i).indices
    } yield {
      if (grid(i)(j) == 'v') {
        val (destI, destJ) = ((i + 1) % rows, j)
        if (grid(destI)(destJ) == '.') {
          Some((i, j) -> (destI, destJ))
        } else {
          None
        }
      } else {
        None
      }
    }).flatten
  }


  def simulate(grid: Array[Array[Char]]): Int = {
    def applyMoves(moves: Seq[((Int, Int), (Int, Int))]): Unit = {
      moves.foreach({
        case ((sourceI, sourceJ), (destI, destJ)) =>
          grid(destI)(destJ) = grid(sourceI)(sourceJ)
          grid(sourceI)(sourceJ) = '.'
      })
    }

    var steps = 1
    var movesForEastHerd = getEastHerdMovements(grid)
    var movesForSouthHerd = getSouthHerdMovements(grid)
    while (movesForEastHerd.nonEmpty || movesForSouthHerd.nonEmpty) {
      applyMoves(movesForEastHerd)
      if (movesForEastHerd.nonEmpty) {
        movesForSouthHerd = getSouthHerdMovements(grid)
      }
      applyMoves(movesForSouthHerd)
      movesForEastHerd = getEastHerdMovements(grid)
      movesForSouthHerd = getSouthHerdMovements(grid)
      steps += 1
    }

    steps
  }

  def parseInput(filePath: String): Array[Array[Char]] = {
    Source.fromResource(filePath).getLines().toArray.map(_.toCharArray)
  }

  def main(args: Array[String]): Unit = {
    val gridTest = parseInput("input_2021_25_test.txt")
    println(simulate(gridTest))
    val grid = parseInput("input_2021_25.txt")
    println(simulate(grid))
  }
}
