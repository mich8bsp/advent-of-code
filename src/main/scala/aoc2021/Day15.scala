package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day15 {

  def findLowestTotalRisk(grid: Array[Array[Int]]): Int = {
    val sideLength: Int = grid.length

    def isOnGrid(pos: (Int, Int)): Boolean = pos._1 >= 0 && pos._1 < sideLength && pos._2 >= 0 && pos._2 < sideLength

    def aStar(start: (Int, Int), goal: (Int, Int)): Int = {
      val costMap: mutable.Map[(Int, Int), Int] = mutable.Map().withDefaultValue(Int.MaxValue)
      costMap(start) = 0

      val positionsToCheck: mutable.PriorityQueue[((Int, Int), Int)] = mutable.PriorityQueue[((Int, Int), Int)]()((a1, a2) => -1 * a1._2.compare(a2._2))
      positionsToCheck.addOne((start, costMap(start)))

      while(positionsToCheck.nonEmpty) {
        val (current, priority) = positionsToCheck.dequeue()
        if (priority == costMap(current)) {
          if (current == goal) {
            return costMap(current)
          }

          val (i, j) = current

          val neighbors = Seq((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
            .filter(isOnGrid)

          neighbors.foreach(neighbor => {
            val tentativeCost: Int = costMap(current) + grid(neighbor._1)(neighbor._2)
            if (tentativeCost < costMap(neighbor)) {
              costMap(neighbor) = tentativeCost
              positionsToCheck.addOne((neighbor, costMap(neighbor)))
            }
          })
        }
      }

      Int.MaxValue
    }

    aStar(start = (0, 0), goal = (sideLength-1, sideLength-1))
  }

  def parseInput(filePath: String): Array[Array[Int]] = {
    Source.fromResource(filePath).getLines().toArray
      .map(_.toArray.map(_.asDigit))
  }

  def expandGrid(grid: Array[Array[Int]]): Array[Array[Int]] = {
    val sideLength: Int = grid.length
    (0 until sideLength*5).map(row => {
      (0 until sideLength*5).map(col => {
        val newValue = grid(row % sideLength)(col % sideLength) + (row/sideLength) + (col/sideLength)
        if(newValue > 9){
          newValue - 9
        }else{
          newValue
        }
      }).toArray
    }).toArray
  }

  def main(args: Array[String]): Unit = {
    val testGrid = parseInput("aoc2021/input_15_test.txt")

    println(findLowestTotalRisk(testGrid)) // 40

    val grid = parseInput("aoc2021/input_15.txt")

    println(findLowestTotalRisk(grid)) // 415

    val testGridFull = expandGrid(testGrid)

    println(findLowestTotalRisk(testGridFull)) // 315

    val gridFull = expandGrid(grid)

    println(findLowestTotalRisk(gridFull)) // 2864
  }
}
