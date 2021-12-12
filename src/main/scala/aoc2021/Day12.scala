package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day12 {

  case class Cave(id: String, neighbors: mutable.Set[Cave] = mutable.Set[Cave]())

  private def parseInput(filePath: String): Cave = {
    val lines = Source.fromResource(filePath).getLines().toList
    val connections: Seq[(String, String)] = lines.map(_.split("-")).map(x => x.head -> x(1))

    val caves: Map[String, Cave] = (connections.map(_._1) ++ connections.map(_._2)).toSet[String].map(x => x -> Cave(x)).toMap
    connections.foreach({
      case (from, to) => {
        val fromCave: Cave = caves(from)
        val toCave: Cave = caves(to)
        fromCave.neighbors.add(toCave)
        toCave.neighbors.add(fromCave)
      }
    })

    caves("start")
  }

  def countCavePaths(start: Cave): Int = {
    def inner(currCave: Cave, visitedSmallCaves: Set[String]): Int = {
      if(currCave.id == "end"){
        1
      }else{
        val neighborsToSearch: Seq[Cave] = currCave.neighbors.filter(x => x.id.forall(_.isUpper) || !visitedSmallCaves.contains(x.id)).toSeq
        if(neighborsToSearch.isEmpty){
          0
        }else{
          neighborsToSearch.map(neighbor => {
            if(neighbor.id.forall(_.isUpper)){
              inner(neighbor, visitedSmallCaves)
            }else{
              inner(neighbor, visitedSmallCaves.+(neighbor.id))
            }
          }).sum
        }
      }
    }

    inner(start, Set("start"))
  }

  def countCavePaths2(start: Cave): Int = {
    def inner(currCave: Cave, visitedSmallCaves: Set[String], smallCaveVisitedTwice: Boolean): Int = {
      if(currCave.id == "end"){
        1
      }else{
        val neighborsToSearch: Seq[Cave] = currCave.neighbors.filterNot(_.id == "start").toSeq
        if(neighborsToSearch.isEmpty){
          0
        }else{
          neighborsToSearch.map(neighbor => {
            if(neighbor.id.forall(_.isUpper)){
              inner(neighbor, visitedSmallCaves, smallCaveVisitedTwice)
            }else{
              (smallCaveVisitedTwice, visitedSmallCaves.contains(neighbor.id)) match {
                case (false, true) => inner(neighbor, visitedSmallCaves, smallCaveVisitedTwice = true)
                case (true, true) => 0
                case _ => inner(neighbor, visitedSmallCaves.+(neighbor.id), smallCaveVisitedTwice)
              }
            }
          }).sum
        }
      }
    }

    inner(start, Set("start"), smallCaveVisitedTwice = false)
  }

  def main(args: Array[String]): Unit = {
    val cavesTest = parseInput("input_2021_12_test.txt")
    println(countCavePaths(cavesTest))
    val caves = parseInput("input_2021_12.txt")
    println(countCavePaths(caves))

    println(countCavePaths2(cavesTest))
    println(countCavePaths2(caves))
  }
}
