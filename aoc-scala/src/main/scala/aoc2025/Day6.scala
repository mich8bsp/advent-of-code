package aoc2025

object Day6 {

  def solve(problems: List[Array[Int]], ops: Array[String]): Long = {
    ops.zipWithIndex.map {
      case ("+", idx) => problems.map(_(idx).toLong).sum
      case ("*", idx) => problems.map(_(idx).toLong).product
    }.sum
  }

  def parseSections(lines: List[String]): Seq[List[String]] = {
    val separationCols = (0 until lines.head.length).filter { i =>
      lines.map(_(i)).toSet == Set(' ')
    }
    val sectionStartEnd = List((0, separationCols.head)) ++ separationCols.sliding(2).map {
      twoCols => (twoCols.head + 1, twoCols.last)
    }.toList ++ List((separationCols.last + 1, lines.map(_.length).max))

    val sections: Seq[List[String]] = sectionStartEnd.map {
      case (start, end) =>
        lines.map(_.slice(start, end))
    }

    sections
  }

  def solve2(sections: Seq[List[String]], ops: Array[String]): Long = {
    sections.zip(ops).map {
      case (section, op) =>
        (0 until section.map(_.length).max).map { i =>
          section.map(line => if (i >= line.length) "" else line(i) ).mkString.trim.toLong
        }.reduce { (a: Long, b: Long) =>
         op match {
            case "+" => a + b
            case "*" => a * b
          }
        }
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val lines = readFileLines(6)(identity)
    val problems = lines.dropRight(1).map { line =>
      line.split(" ").filter(_.nonEmpty).map(_.toInt)
    }
    val ops = lines.last.split(" ").filter(_.nonEmpty)
    println(solve(problems, ops))

    val sections = parseSections(lines.dropRight(1))
    println(solve2(sections, ops))
  }
}
