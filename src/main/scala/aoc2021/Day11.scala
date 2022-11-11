package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day11 {

  def parseInput(lines: List[String]): Array[Array[Int]] = {
    lines.map(_.toCharArray.map(_.asDigit)).toArray
  }

  def stepIncrement(grid: Array[Array[Int]]): Unit = {
    for{
      i <- grid.indices
      j <- grid(i).indices
    }yield{
      grid(i)(j) = grid(i)(j) + 1
    }
  }

  def stepFlash(grid: Array[Array[Int]]): Seq[(Int, Int)] = {
    val flashers: mutable.Set[(Int, Int)] = mutable.Set()
    val positionsToFlash: mutable.Stack[(Int, Int)] = mutable.Stack[(Int, Int)]()
    for{
      i <- grid.indices
      j <- grid(i).indices
    }yield{
      if(grid(i)(j) > 9){
        positionsToFlash.push((i, j))
        flashers.add((i, j))
      }
    }

    while (positionsToFlash.nonEmpty){
      val currFlasher = positionsToFlash.pop()
      val neighborsToAffect: Seq[(Int, Int)] = (for{
        di <- Seq(-1, 0, 1)
        dj <- Seq(-1, 0 ,1)
        i = currFlasher._1
        j =  currFlasher._2
      }yield {
        val neighborI = i + di
        val neighborJ = j + dj
        if(neighborI>=0 && neighborI<grid.length && neighborJ >= 0 && neighborJ < grid.head.length &&
          (neighborI, neighborJ) != (i, j) && !flashers.contains((neighborI, neighborJ))){
          Some(neighborI, neighborJ)
        }else{
          None
        }
      }).flatten

      neighborsToAffect.foreach({
        case (i, j) =>
          grid(i)(j) = grid(i)(j) + 1
          if(grid(i)(j) > 9){
            positionsToFlash.push((i, j))
            flashers.add((i, j))
          }
      })
    }

    flashers.toSeq
  }

  def stepResetFlashed(grid: Array[Array[Int]], flashLocations: Seq[(Int, Int)]): Unit = {
    flashLocations.foreach({
      case (i, j) => grid(i)(j) = 0
    })
  }

  def getNumberOfFlashes(grid: Array[Array[Int]], steps: Int): Int = {
    (0 until steps).map(_ => {
      stepIncrement(grid)
      val flashLocations = stepFlash(grid)
      stepResetFlashed(grid, flashLocations)
      flashLocations.size
    }).sum
  }

  def getFirstSynchronizedStep(grid: Array[Array[Int]]): Int = {
    var steps = 0
    while (true){
      steps += 1
      stepIncrement(grid)
      val flashLocations = stepFlash(grid)
      if(flashLocations.size == (grid.length * grid.head.length)){
        return steps
      }
      stepResetFlashed(grid, flashLocations)
    }
    0
  }

  def main(args: Array[String]): Unit = {
    var testGrid = parseInput(Source.fromResource("aoc2021/input_11_test.txt").getLines().toList)
    println(getNumberOfFlashes(testGrid, steps = 100))

    var grid = parseInput(Source.fromResource("aoc2021/input_11.txt").getLines().toList)

    println(getNumberOfFlashes(grid, steps = 100))

    testGrid = parseInput(Source.fromResource("aoc2021/input_11_test.txt").getLines().toList)
    grid = parseInput(Source.fromResource("aoc2021/input_11.txt").getLines().toList)

    println(getFirstSynchronizedStep(testGrid))
    println(getFirstSynchronizedStep(grid))
  }
}
