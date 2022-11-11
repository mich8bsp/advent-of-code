package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day9 {

  def isValidPoint(p: (Int, Int))
                  (implicit heightMap: Array[Array[Int]]): Boolean = {
    val rows: Int = heightMap.length
    val cols: Int = heightMap.head.length
    p._1 >= 0 && p._2>=0 && p._1 < rows && p._2 < cols
  }

  def getLowPointLocations(heightMap: Array[Array[Int]]): Seq[(Int, Int)] = {
    implicit val implMap = heightMap
    val rows: Int = heightMap.length
    val cols: Int = heightMap.head.length

    def isLowPoint(i: Int, j: Int, neighbors: Seq[(Int, Int)]): Boolean = {
      neighbors.forall({
        case (neighborRow, neighborCol) => heightMap(neighborRow)(neighborCol) > heightMap(i)(j)
      })
    }

   (for {
      i <- 0 until rows
      j <- 0 until cols
      neighbourIndices = Seq((i-1,j), (i+1, j), (i, j-1), (i, j+1))
        .filter(isValidPoint)
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
    implicit val implMap = heightMap

    val pointsInBasin: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
    val bfsQueue: mutable.Queue[(Int, Int)] = mutable.Queue[(Int, Int)]()

    pointsInBasin.add(startPoint)
    bfsQueue.addOne(startPoint)

    while(bfsQueue.nonEmpty){
      val curr = bfsQueue.dequeue()
      val i = curr._1
      val j = curr._2
      val neighborsDiscovered = Seq((i-1,j), (i+1, j), (i, j-1), (i, j+1))
        .filter(isValidPoint)
        .filterNot(pointsInBasin.contains)
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
    val heightMapTest: Array[Array[Int]] = parseInput("aoc2021/input_9_test.txt")
    val heightMap: Array[Array[Int]] = parseInput("aoc2021/input_9.txt")

    println(getRiskLevel(heightMapTest))
    println(getRiskLevel(heightMap))

    println(getMultiBasinSize(heightMapTest))
    println(getMultiBasinSize(heightMap))
  }
}
