package aoc2022

import scala.annotation.tailrec

object Day12 {
  implicit class Tuple2Addable(inner: (Int, Int)) {
    def +(other: (Int, Int)): (Int, Int) = {
      (inner._1 + other._1, inner._2 + other._2)
    }
  }

  def findShortestPath(heightMap: Array[Array[Char]], startPos: (Int, Int)): Int = {
    val rows = heightMap.length
    val cols = heightMap.head.length

    def getHeightAt(idx: (Int, Int)): Option[Char] = {
      val (r, c) = idx
      for {
        normalizedRow <- Some(r).filter(_ >= 0).filter(_ < rows)
        normalizedCol <- Some(c).filter(_ >= 0).filter(_ < cols)
      } yield {
        heightMap(normalizedRow)(normalizedCol)
      }
    }

    @tailrec
    def search(currLayer: List[(Int, Int)], visited: Set[(Int, Int)], pathLength: Int = 0): Int =  {
      currLayer match {
        case Nil => Int.MaxValue
        case _ => if (currLayer.flatMap(getHeightAt).contains('E')) {
          pathLength
        } else {
          val updatedVisited = visited ++ currLayer
          val updatedPathLength = pathLength + 1
          val nextLayer = for {
            delta <- List((0, 1), (1, 0), (0, -1), (-1, 0))
            currPos <- currLayer
            currPosHeight <- getHeightAt(currPos)
            nextPos = currPos + delta if !visited.contains(nextPos)
            nextPosHeight <- getHeightAt(nextPos) if currPosHeight == 'S' || nextPosHeight - currPosHeight < 2
          } yield {
            nextPos
          }
          search(nextLayer.distinct, updatedVisited, updatedPathLength)
        }
      }
    }

    search(List(startPos), Set.empty[(Int, Int)])
  }

  def solveA(heightMap: Array[Array[Char]]): Int = {
    val startRow = heightMap.indexWhere(_.contains('S'))
    val startCol = heightMap(startRow).indexOf('S')
    findShortestPath(heightMap, (startRow, startCol))
  }

  def solveB(heightMap: Array[Array[Char]]): Int = {
    val startingPositions = for {
      i <- heightMap.indices
      j <- heightMap(i).indices if Set('S', 'a').contains(heightMap(i)(j))
    } yield {
      (i, j)
    }

    startingPositions.map(findShortestPath(heightMap, _))
      .min
  }

  def main(args: Array[String]): Unit = {
    val heightMap = readCharGrid(12)
    println(solveA(heightMap))
    println(solveB(heightMap))
  }
}
