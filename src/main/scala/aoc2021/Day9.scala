package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day9 {

  def getLowPointLocations(heightMap: Array[Array[Int]]): Seq[(Int, Int)] = {
    val rows: Int = heightMap.length
    val cols: Int = heightMap.head.length

    def isValidPoint(i: Int, j: Int): Boolean = {
      i >= 0 && j>=0 && i < rows && j < cols
    }

    def isLowPoint(i: Int, j: Int, neighbors: Seq[(Int, Int)]): Boolean = {
      neighbors.forall({
        case (neighborRow, neighborCol) => heightMap(neighborRow)(neighborCol) > heightMap(i)(j)
      })
    }

   (for {
      i <- 0 until rows
      j <- 0 until cols
      neighbourIndices = Seq((i-1,j), (i+1, j), (i, j-1), (i, j+1)).filter({
        case (neighborRow, neighborCol) => isValidPoint(neighborRow, neighborCol)
      })
    } yield {
      if(isLowPoint(i, j, neighbourIndices)){
        Some((i, j))
      }else{
        None
      }
    }).flatten
  }

  def getRiskLevel(heightMap: Array[Array[Int]]): Int = {
    getLowPointLocations(heightMap)
      .map({
        case (i, j) => heightMap(i)(j)
      }).map(_ + 1).sum
  }

  def getBasin(heightMap: Array[Array[Int]], startPoint: (Int, Int)): Set[(Int, Int)] = {
    val rows: Int = heightMap.length
    val cols: Int = heightMap.head.length
    val pointsInBasin: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
    val bfsQueue: mutable.Queue[(Int, Int)] = mutable.Queue[(Int, Int)]()

    pointsInBasin.add(startPoint)
    bfsQueue.addOne(startPoint)

    def isValidPoint(i: Int, j: Int): Boolean = {
      i >= 0 && j>=0 && i < rows && j < cols
    }

    while(bfsQueue.nonEmpty){
      val curr = bfsQueue.dequeue()
      val i = curr._1
      val j = curr._2
      val neighborsDiscovered = Seq((i-1,j), (i+1, j), (i, j-1), (i, j+1))
        .filter(x => isValidPoint(x._1, x._2))
        .filter(x => !pointsInBasin.contains(x))
        .filter({
          case (neighborI, neighborJ) => heightMap(neighborI)(neighborJ) < 9
        })

      pointsInBasin.addAll(neighborsDiscovered)
      bfsQueue.enqueueAll(neighborsDiscovered)
    }

    pointsInBasin.toSet
  }

  def getMultiBasinSize(heightMap: Array[Array[Int]]): Int = {
    val lowPoints: Seq[(Int, Int)] = getLowPointLocations(heightMap)

    val basins: Seq[Set[(Int, Int)]] = lowPoints.map(lowPoint => getBasin(heightMap, lowPoint))

    basins.map(_.size).sorted.reverse.take(3).product
  }

  private def parseInput(filePath: String): Array[Array[Int]] = {
    Source.fromResource(filePath).getLines().toArray
      .map(line => line.toCharArray.map(_.asDigit))
  }

  def main(args: Array[String]): Unit = {
    val heightMapTest: Array[Array[Int]] = parseInput("input_2021_9_test.txt")
    val heightMap: Array[Array[Int]] = parseInput("input_2021_9.txt")

    println(getRiskLevel(heightMapTest))
    println(getRiskLevel(heightMap))

    println(getMultiBasinSize(heightMapTest))
    println(getMultiBasinSize(heightMap))
  }
}
