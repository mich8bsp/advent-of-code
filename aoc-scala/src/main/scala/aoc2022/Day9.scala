package aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day9 {

  object Direction extends Enumeration {
    val LEFT, RIGHT, UP, DOWN = Value
  }

  def findNextTailPos(currTail: (Int, Int), currHead: (Int, Int)): (Int, Int) = {
    val (tailRow, tailCol) = currTail
    val (headRow, headCol) = currHead

    (math.abs(tailRow - headRow), math.abs(tailCol - headCol)) match {
      case (2, 2) => ((headRow + tailRow) / 2, (headCol + tailCol) / 2)
      case (2, _) => ((headRow + tailRow) / 2, headCol)
      case (_, 2) => (headRow, (headCol + tailCol) / 2)
      case _ => currTail
    }
  }

  def findNextHeadPos(currHead: (Int, Int), direction: Direction.Value): (Int, Int) = {
    (currHead, direction) match {
      case ((headRow, headCol), Direction.UP) => (headRow - 1, headCol)
      case ((headRow, headCol), Direction.DOWN) => (headRow + 1, headCol)
      case ((headRow, headCol), Direction.LEFT) => (headRow, headCol - 1)
      case ((headRow, headCol), Direction.RIGHT) => (headRow, headCol + 1)
    }
  }

  def findNumTailVisited(commands: List[Direction.Value], numOfKnots: Int): Int = {
    val tailVisitedPositions: mutable.Set[(Int, Int)] = mutable.Set.empty[(Int, Int)]

    @tailrec
    def simulate(commandsLeft: List[Direction.Value], knots: List[(Int, Int)]): Unit = {
      tailVisitedPositions.add(knots.last)

      commandsLeft match {
        case Nil => ()
        case command :: rest =>
          val updatedKnots = knots.foldLeft(List.empty[(Int, Int)]) {
            case (knotsMoved, currKnot) => if (knotsMoved.isEmpty) {
              List(findNextHeadPos(currKnot, command))
            } else {
              knotsMoved appended findNextTailPos(currKnot, knotsMoved.last)
            }
          }
          simulate(rest, updatedKnots)
      }
    }

    simulate(commands, knots = (0 until numOfKnots).map(_ => (0, 0)).toList)
    tailVisitedPositions.size
  }

  def solveA(commands: List[Direction.Value]): Int = {
    findNumTailVisited(commands, 2)
  }

  def solveB(commands: List[Direction.Value]): Int = {
    findNumTailVisited(commands, 10)
  }

  def main(args: Array[String]): Unit = {
    val commands = readFileLines(9) { line =>
      val Array(dirStr, stepsStr) = line.split(" ")
      val direction: Direction.Value = dirStr match {
        case "L" => Direction.LEFT
        case "R" => Direction.RIGHT
        case "U" => Direction.UP
        case "D" => Direction.DOWN
      }
      (stepsStr.toInt, direction)
    }.flatMap {
      case (steps, direction) => (0 until steps).map(_ => direction)
    }

    println(solveA(commands))
    println(solveB(commands))
  }
}
