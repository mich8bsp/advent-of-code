package aoc2020

import scala.collection.mutable

object Day10 {

  def solveA(adapters: List[Int]): Int = {
    val joltageDifferenceToCount = (adapters ++ List(0, adapters.max + 3))
      .sorted
      .sliding(2)
      .map(x => x.last - x.head)
      .toList
      .groupBy(identity)
      .view
      .mapValues(_.size)

    joltageDifferenceToCount.getOrElse(1, 0) * joltageDifferenceToCount.getOrElse(3, 0)
  }

  def solveB(adapters: List[Int]): Long = {
    val allJoltages = (adapters ++ List(0, adapters.max + 3)).sorted.toArray
    val cache: mutable.Map[(Int, Int), Long] = mutable.Map.empty

    def countConnectionsFrom(lastConnected: Int, idx: Int): Long = cache.getOrElseUpdate((lastConnected, idx), {
      if (allJoltages(idx) - lastConnected > 3) {
        0
      } else {
        if (idx == allJoltages.length - 1) {
          1
        } else {
          countConnectionsFrom(allJoltages(idx), idx + 1) +
          countConnectionsFrom(lastConnected, idx + 1)
        }
      }
    })

    countConnectionsFrom(0, 1)
  }

  def main(args: Array[String]): Unit = {
    val adapters = readFileLines[Int](10)
    println(solveA(adapters))
    println(solveB(adapters))
  }
}
