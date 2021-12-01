package aoc2021

import scala.io.Source

object Day1 {

  def groupByThree(readings: List[Int]): List[Int] = {
    (0 until readings.length - 2).map(i => {
      readings(i) + readings(i+1) + readings(i+2)
    }).toList
  }

  def calculateIncreases(readings: List[Int]): Int = {
    readings.foldLeft((readings.head, 0))({
      case ((prev, increases), curr) => (curr, if(curr > prev) increases + 1 else increases)
    })._2
  }

  def main(args: Array[String]): Unit = {
    val readings = Source.fromResource("input2021_1.txt").getLines().map(_.toInt).toList
    println(calculateIncreases(readings)) //1342

    println(calculateIncreases(groupByThree(readings))) //1378
  }
}
