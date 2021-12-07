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
    var positions = Source.fromResource("input_2021_7_test.txt").getLines().mkString.split(",").map(_.toInt).toList

    val constCostFunction = (distance: Int) => distance
    val rampCostFunction = (distance: Int) => (1 + distance) * distance / 2
    println(calculateAlignmentCost(positions)(constCostFunction))

    positions = Source.fromResource("input_2021_7.txt").getLines().mkString.split(",").map(_.toInt).toList

    println(calculateAlignmentCost(positions)(rampCostFunction))

  }
}
