package aoc2021

import scala.io.Source

object Day8 {

  def decodeDigits(combinations: Array[String]): Map[Set[Char], Int] = {
    // 2 is 1
    // 3 is 7
    // 4 is 4
    // 5 is 2 or 5 or 3
    // 6 is 0 or 6 or 9
    // 7 is 8

    val signalsInOne: Set[Char] = combinations.find(_.size == 2).get.toCharArray.toSet
    val signalsInSeven: Set[Char] = combinations.find(_.size == 3).get.toCharArray.toSet
    val signalsInFour: Set[Char] = combinations.find(_.size == 4).get.toCharArray.toSet
    val signalsInEight: Set[Char] = combinations.find(_.size == 7).get.toCharArray.toSet

    val topLeftAndMiddle: Set[Char] = signalsInFour -- signalsInOne

    val signalsInFive: Set[Char] = combinations.filter(_.size == 5).map(_.toCharArray.toSet)
      .filter(x => topLeftAndMiddle.subsetOf(x)).head
    val signalsInThree: Set[Char] = combinations.filter(_.size == 5).map(_.toCharArray.toSet)
      .filter(x => signalsInOne.subsetOf(x)).head
    val signalsInTwo: Set[Char] = combinations.filter(_.size == 5).map(_.toCharArray.toSet)
      .filterNot(_ == signalsInThree)
      .filterNot(_ == signalsInFive).head

    val signalsInNine: Set[Char] = combinations.filter(_.size == 6).map(_.toCharArray.toSet)
      .filter(x => signalsInFive.subsetOf(x))
      .filter(x => signalsInOne.subsetOf(x)).head

    val signalsInZero: Set[Char] = combinations.filter(_.size == 6).map(_.toCharArray.toSet)
      .filterNot(_ == signalsInNine)
      .filter(x => signalsInOne.subsetOf(x)).head

    val signalsInSix: Set[Char] = combinations.filter(_.size == 6).map(_.toCharArray.toSet)
      .filterNot(_ == signalsInNine)
      .filterNot(_ == signalsInZero).head

    Map(
      signalsInZero -> 0,
      signalsInOne -> 1,
      signalsInTwo -> 2,
      signalsInThree -> 3,
      signalsInFour -> 4,
      signalsInFive -> 5,
      signalsInSix -> 6,
      signalsInSeven -> 7,
      signalsInEight -> 8,
      signalsInNine -> 9
    )
  }

  def countDigitsOfInterest(combinations: Array[String], outputValues: Array[String], digitsToCount: Set[Int]): Int = {
    val decodingLegend: Map[Set[Char], Int] = decodeDigits(combinations)

    outputValues.map(_.toCharArray.toSet)
      .map(x => decodingLegend(x))
      .count(x => digitsToCount.contains(x))
  }

  def decodeOutput(combinations: Array[String], outputValues: Array[String]): Int = {
    val decodingLegend: Map[Set[Char], Int] = decodeDigits(combinations)

    outputValues.map(_.toCharArray.toSet)
      .map(x => decodingLegend(x))
      .mkString
      .toInt
  }

  def parseInput(filePath: String): Seq[(Array[String], Array[String])] = {
    val lines = Source.fromResource(filePath).getLines()
    lines.map(line => {
      val Array(digitCombinations, outputValues) = line.split("\\|").map(_.split(" ").filter(_.nonEmpty))
      (digitCombinations, outputValues)
    }).toList
  }

  def part1(): Unit ={
    var entries = parseInput("aoc2021/input_8_test.txt")
    var res = entries.map(entry => {
      countDigitsOfInterest(entry._1, entry._2, digitsToCount = Set(1,4,7,8))
    }).sum
    println(res)

    entries = parseInput("aoc2021/input_8.txt")
    res = entries.map(entry => {
      countDigitsOfInterest(entry._1, entry._2, digitsToCount = Set(1,4,7,8))
    }).sum



    println(res)
  }

  def part2(): Unit = {
    var entries = parseInput("aoc2021/input_8_test.txt")
    var res = entries.map(entry => {
      decodeOutput(entry._1, entry._2)
    }).sum
    println(res)

    entries = parseInput("aoc2021/input_8.txt")
    res = entries.map(entry => {
      decodeOutput(entry._1, entry._2)
    }).sum

    println(res)
  }

  def main(args: Array[String]): Unit = {
//    part1()
    part2()
  }
}
