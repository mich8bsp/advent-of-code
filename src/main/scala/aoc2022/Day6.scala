package aoc2022

object Day6 {

  def solve(windowSize: Int)(input: String): Int = {
    input.zipWithIndex
      .sliding(windowSize)
      .find(_.map(_._1).toSet.size == windowSize)
      .map(_.last._2)
      .get + 1
  }

  def main(args: Array[String]): Unit = {
    val input = readFileLines(6)(identity).head
    val solveA = solve(4) _
    val solveB = solve(14) _
    println(solveA(input))
    println(solveB(input))
  }
}