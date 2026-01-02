package aoc2022

object Day4 {

  def solveA(assignments: List[(Range, Range)]): Int = {
    assignments.count {
      case (first, second) => (first.start <= second.start && first.end >= second.end) ||
        (second.start <= first.start && second.end >= first.end)
    }
  }

  def solveB(assignments: List[(Range, Range)]): Int = {
    assignments.count {
      case (first, second) => (first.start <= second.end && first.end >= second.start) ||
        (second.start <= first.end && second.end >= first.start)
    }
  }

  private def parseRange(str: String): Range = {
    val Array(start, end) = str.split("-").map(_.toInt)
    start to end
  }
  def main(args: Array[String]): Unit = {
    val assignments = readFileLines[(Range, Range)](4) { line =>
      val Array(firstStr, secondStr) = line.split(",")
      (parseRange(firstStr), parseRange(secondStr))
    }

    println(solveA(assignments))
    println(solveB(assignments))
  }
}
