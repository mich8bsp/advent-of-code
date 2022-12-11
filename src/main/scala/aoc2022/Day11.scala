package aoc2022

import scala.collection.mutable
import scala.util.Try

object Day11 {
  type MonkeyId = Int
  type WorryLevel = Long

  case class Monkey(id: MonkeyId, monkeyInspectionWorryLevelBoost: WorryLevel => WorryLevel, testDivisibleBy: WorryLevel, testPassedMonkey: MonkeyId, testFailedMonkey: MonkeyId) {
    private val itemsQueue: mutable.Queue[WorryLevel] = mutable.Queue.empty[WorryLevel]
    var inspected: Long = 0L

    def addItem(item: WorryLevel): Unit = {
      itemsQueue.append(item)
    }

    def inspectItems(implicit postInspectionWorryLevelReduction: WorryLevel => WorryLevel): Map[MonkeyId, Seq[WorryLevel]] = {
      itemsQueue.removeAll().map { item =>
        inspected += 1
        val updatedWorryLevel = postInspectionWorryLevelReduction(monkeyInspectionWorryLevelBoost(item))
        if (updatedWorryLevel % testDivisibleBy == 0) {
          testPassedMonkey -> updatedWorryLevel
        } else {
          testFailedMonkey -> updatedWorryLevel
        }
      }.groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap
    }

    def print(): Unit = {
      println(s"Monkey $id: (${inspected}) ${itemsQueue.mkString(", ")}")
    }
  }

  def parseMonkey(lines: List[String]): Monkey = {
    val monkeyId: MonkeyId = lines.head.split(" ")(1).filter(_.isDigit).toInt
    val startingItems: Array[WorryLevel] = lines(1).split(":")(1).split(",").map(_.trim).map(_.toLong)
    val opF: WorryLevel => WorryLevel = {
      val opExpression: String = lines(2).split(":")(1)
      opExpression.split("=")(1).split(" ").filter(_.nonEmpty).toList match {
        case "old" :: "+" :: "old" :: Nil => (x: WorryLevel) => x + x
        case "old" :: "-" :: "old" :: Nil => (x: WorryLevel) => x - x
        case "old" :: "*" :: "old" :: Nil => (x: WorryLevel) => x * x
        case "old" :: "/" :: "old" :: Nil => (x: WorryLevel) => x / x
        case "old" :: "+" :: const :: Nil if Try(const.toInt).isSuccess => (x: WorryLevel) => x + const.toInt
        case "old" :: "-" :: const :: Nil if Try(const.toInt).isSuccess => (x: WorryLevel) => x - const.toInt
        case "old" :: "*" :: const :: Nil if Try(const.toInt).isSuccess => (x: WorryLevel) => x * const.toInt
        case "old" :: "/" :: const :: Nil if Try(const.toInt).isSuccess => (x: WorryLevel) => x / const.toInt
      }
    }
    val testDivisibleBy: WorryLevel = lines(3).split(":")(1).split(" ").filter(_.nonEmpty).toList match {
        case "divisible" :: "by" :: const :: Nil => const.toInt
      }


    val ifTrueMonkey: MonkeyId = lines(4).split(" ").last.toInt
    val ifFalseMonkey: MonkeyId = lines(5).split(" ").last.toInt

    val monkey = Monkey(monkeyId, opF, testDivisibleBy, ifTrueMonkey, ifFalseMonkey)
    startingItems.foreach(monkey.addItem)
    monkey
  }


  def calculateMonkeyBusiness(monkeys: List[Monkey], rounds: Int)
                             (implicit postInspectionWorryLevelReduction: WorryLevel => WorryLevel): Long = {
    val monkeysById: Map[MonkeyId, Monkey] = monkeys.map(x => x.id -> x).toMap
    val combinedDivisibleTest: WorryLevel = monkeys.map(_.testDivisibleBy).product
    (0 until rounds).foreach { roundsIdx =>
      monkeys.foreach { monkey =>
        val output: Map[MonkeyId, Seq[WorryLevel]] = monkey.inspectItems
        output.foreach {
          case (monkeyId: MonkeyId, items: Seq[WorryLevel]) => items
            .map(_ % combinedDivisibleTest)
            .foreach(monkeysById(monkeyId).addItem)
        }
      }

//      println(s"Round ${roundsIdx+1}")
//      monkeys.foreach(_.print())
    }
    monkeys.map(_.inspected)
      .sorted(Ordering.Long.reverse)
      .take(2)
      .product
  }

  def solveA(monkeys: List[Monkey]): Long = {
    implicit val postInspectionWorryLevelReduction: WorryLevel => WorryLevel = _ / 3
    calculateMonkeyBusiness(monkeys, 20)
  }

  def solveB(monkeys: List[Monkey]): Long = {
    implicit val postInspectionWorryLevelReduction: WorryLevel => WorryLevel = identity
    calculateMonkeyBusiness(monkeys, 10000)
  }

  def main(args: Array[String]): Unit = {
    println(solveA(readSections(11)(parseMonkey)))
    println(solveB(readSections(11)(parseMonkey)))
  }
}
