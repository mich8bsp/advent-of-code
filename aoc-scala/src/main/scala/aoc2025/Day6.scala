package aoc2025

object Day6 {

  def solve(problems: List[Array[Int]], ops: Array[String]): Long = {
    ops.zipWithIndex.map {
      case ("+", idx) => problems.map(_(idx).toLong).sum
      case ("*", idx) => problems.map(_(idx).toLong).product
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val lines = readFileLines(6)(identity)
    val problems = lines.dropRight(1).map { line =>
      line.split(" ").filter(_.nonEmpty).map(_.toInt)
    }
    val ops = lines.last.split(" ").filter(_.nonEmpty)
    println(solve(problems, ops))
  }
}
