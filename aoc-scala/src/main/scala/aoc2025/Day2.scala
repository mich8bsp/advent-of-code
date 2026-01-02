package aoc2025

object Day2 {

  def solve(ids: Seq[(Long, Long)]): Seq[Long] = {
    val invalidIds = ids.flatMap {
      case (start, end) =>
        (start to end).flatMap { n =>
          val numAsString: String = n.toString
          if (numAsString.length % 2 == 0) {
            if (numAsString.substring(0, numAsString.length / 2) == numAsString.substring(numAsString.length / 2, numAsString.length)) {
              Some(n)
            } else {
              None
            }
          } else {
            None
          }
        }
    }

    invalidIds
  }

  def solve2(ids: Seq[(Long, Long)]): Seq[Long] = {
    val invalidIds = ids.flatMap {
      case (start, end) =>
        (start to end).flatMap { n =>
          val numAsString: String = n.toString
          val isInvalid = (1 to numAsString.length / 2).reverse.exists { repeatLength =>
            numAsString.substring(0, repeatLength) * (numAsString.length / repeatLength) == numAsString
          }
          if (isInvalid) {
            Some(n)
          } else {
            None
          }
        }
    }

    invalidIds
  }

  def main(args: Array[String]): Unit = {
    val idRanges: Seq[(Long, Long)] = readFileLines(2) { line =>
      line.split(",").map(range => (range.split("-").head.toLong, range.split("-")(1).toLong))
        .toList
    }.head

    println(solve(idRanges).sum)
    println(solve2(idRanges).sum)
  }
}
