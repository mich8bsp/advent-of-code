package aoc2020

import scala.collection.mutable

object Day7 {

  case class Rule(outerBag: String, innerBags: Map[String, Int])

  def parseRules: List[Rule] = {
    readFileLines[Rule](7) { line =>
      val Array(outer, inner) = line.split(" contain ")
      val outerBagColor: String = outer.slice(0, outer.indexOf(" bags"))
      val innerBags: Map[String, Int] = inner match {
        case "no other bags." => Map.empty[String, Int]
        case _ =>
          inner.split(", ").map(innerBagStr => {
            val innerBagStrParts = innerBagStr.split(" ")
            val count: Int = innerBagStrParts.head.toInt
            val color: String = innerBagStrParts.drop(1).dropRight(1).mkString(" ")
            color -> count
          }).toMap
      }
      Rule(outerBagColor, innerBags)
    }
  }

  def solveA(rules: List[Rule]): Int = {
    val outerBagsByInner: Map[String, Set[String]] = rules.flatMap(rule =>
      rule.innerBags
        .keySet
        .map(inner => inner -> rule.outerBag)
    ).groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap

    val colorsEncountered: mutable.Set[String] = mutable.Set.empty[String]
    val searchQueue: mutable.Queue[String] = mutable.Queue.empty[String]
    val startColor = "shiny gold"
    colorsEncountered.add(startColor)
    searchQueue.enqueue(startColor)
    while (searchQueue.nonEmpty) {
      val currColor = searchQueue.dequeue()
      val nextColors = outerBagsByInner.getOrElse(currColor, Set.empty[String])
        .diff(colorsEncountered)

      colorsEncountered.addAll(nextColors)
      searchQueue.enqueueAll(nextColors)
    }
    colorsEncountered.filterNot(_ == startColor).size
  }

  def solveB(rules: List[Rule]): Int = {
    val rulesByOuter: Map[String, Rule] = rules.map(rule => rule.outerBag -> rule).toMap
    val cache: mutable.Map[String, Int] = mutable.Map.empty[String, Int]

    def countInnerBagsInBag(outerBagColor: String): Int = {
      cache.getOrElseUpdate(outerBagColor, {
        rulesByOuter(outerBagColor)
          .innerBags
          .map {
            case (innerBagColor, count) => count * (countInnerBagsInBag(innerBagColor) + 1)
          }
          .sum
      })
    }

    countInnerBagsInBag("shiny gold")
  }

  def main(args: Array[String]): Unit = {
    val rules = parseRules
    println(solveA(rules))
    println(solveB(rules))
  }
}
