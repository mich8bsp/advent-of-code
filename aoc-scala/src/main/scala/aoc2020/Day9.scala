package aoc2020

object Day9 {

  def isValid(preamble: List[Long]): Boolean = {
    val sum: Long = preamble.last
    val candidates: Set[Long] = preamble.dropRight(1).toSet
    candidates.exists(n => n != (sum - n) && candidates.contains(sum - n))
  }

  def solveA(numbers: List[Long]): Option[Long] = {
    numbers.sliding(26)
      .find(preamble => !isValid(preamble))
      .map(_.last)
  }

  def solveB(numbers: List[Long]): Option[Long] = {
    val target = solveA(numbers).get
    var left = 0
    var right = 1
    var sum = numbers(left) + numbers(right)
    while (sum != target && numbers(right) < target && left < right) {
      if (sum > target) {
        sum -= numbers(left)
        left += 1
      } else {
        right += 1
        sum += numbers(right)
      }
    }

    Some(numbers.slice(left, right + 1))
      .filter(_ => sum == target)
      .map(range => range.min + range.max)
  }

  def main(args: Array[String]): Unit = {
    val input = readFileLines[Long](9)
    println(solveA(input))
    println(solveB(input))
  }
}
