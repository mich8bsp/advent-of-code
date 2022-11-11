package aoc2021

import scala.io.Source

object Day7 {
  type Distance = Int
  type Position = Int
  type Cost = Int

  def calculateAlignmentCost(positions: List[Position])
                            (costFunction: Distance => Cost): Cost = {

    def calculateAlignmentCostToPosition(alignmentPos: Position): Cost = {
     positions.map(x => costFunction(math.abs(x - alignmentPos))).sum
    }

    (positions.min to positions.max).map(calculateAlignmentCostToPosition).min
  }


  def main(args: Array[String]): Unit = {
    var positions = readFileLines[List[Int]](7, isTest = true).flatten

    val constCostFunction = (distance: Int) => distance
    val rampCostFunction = (distance: Int) => (1 + distance) * distance / 2
    println(calculateAlignmentCost(positions)(constCostFunction))

    positions = readFileLines[List[Int]](7).flatten

    println(calculateAlignmentCost(positions)(rampCostFunction))

  }
}
