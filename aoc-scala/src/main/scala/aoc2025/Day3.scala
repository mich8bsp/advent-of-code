package aoc2025

object Day3 {

  def findJoltage(bank: String): Int = {
    val leftDigit = bank.dropRight(1).max
    val leftDigitIdx = bank.indexOf(leftDigit)
    val rightDigit = bank.slice(leftDigitIdx + 1, bank.length).max
    s"$leftDigit$rightDigit".toInt
  }

  def findJoltage2(bank: String, numDigitsInJoltage: Int): Long = {
    def findJoltageDigit(lastDigitIdx: Int, digitNum: Int): (Int, Char) = {
      val digitsToLeaveFromRight: Int = numDigitsInJoltage - digitNum
      val currDigit: Char = bank.slice(lastDigitIdx + 1, bank.length - digitsToLeaveFromRight).max
      val currDigitIdx: Int = bank.indexOf(currDigit, lastDigitIdx + 1)
      (currDigitIdx, currDigit)
    }
    val (finalJoltage, _) = (1 to numDigitsInJoltage).foldLeft((0L, -1)) {
      case ((currJoltage, lastDigitIdx), digitNum) =>
        val (currDigitIdx, currDigit) = findJoltageDigit(lastDigitIdx, digitNum)
        (currJoltage * 10 + (currDigit - '0'), currDigitIdx)
    }

    finalJoltage
  }

  def main(args: Array[String]): Unit = {
    val banks: Seq[String] = readFileLines(3)(identity)
    println(banks.map(findJoltage).sum)
    println(banks.map(findJoltage2(_, 12)).sum)
  }
}
