package aoc2025

object Day5 {
  private def countFresh(freshRanges: List[(Long, Long)], ingredientIds: Set[Long]) = {
    ingredientIds.count { id =>
      freshRanges.exists {
        case (start, end) => id <= end && id >= start
      }
    }
  }

  private def countFreshFromRanges(freshRanges: List[(Long, Long)]): Long = {
    val sorted = freshRanges
      .map { case (from, to) => if (from <= to) (from, to) else (to, from) }
      .sortBy(_._1)

    val merged = sorted.foldLeft(List.empty[(Long, Long)]) {
      case (Nil, r) => r :: Nil
      case ((currFrom, currTo) :: rest, (from, to)) =>
        if (from <= currTo + 1) {
          (currFrom, math.max(currTo, to)) :: rest
        } else {
          (from, to) :: (currFrom, currTo) :: rest
        }
    }.reverse

    merged.map { case (from, to) => to - from + 1 }.sum
  }

  def main(args: Array[String]): Unit = {
    val lines = readFileLines(5)(identity)
    val freshRanges = lines.takeWhile(_.nonEmpty).map { line =>
      (line.split("-").head.toLong, line.split("-")(1).toLong)
    }
    val ingredientIds: Set[Long] = lines.dropWhile(_.nonEmpty).drop(1).map(_.toLong).toSet
    println(countFresh(freshRanges, ingredientIds))
    println(countFreshFromRanges(freshRanges))
  }

}
