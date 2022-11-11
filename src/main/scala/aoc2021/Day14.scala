package aoc2021

import scala.annotation.tailrec

object Day14 {

  def producePolymer(template: String, rules: Map[(Char, Char), Char], iterations: Int): Map[Char, Long] = {

    @tailrec
    def calculateOccurrencesForPairsAfterIterations(pairsOccurrences: Map[(Char, Char), Long], iterationsLeft: Int): Map[(Char, Char), Long] = {
      if(iterationsLeft == 0){
        pairsOccurrences
      }else {
        val pairsOccurrencesAfterOneIteration: Map[(Char, Char), Long] = pairsOccurrences.toSeq.flatMap({
          case (pair, numOfOccurrences) =>
            rules.get(pair)
              .map(insertionChar => {
                Seq((pair._1, insertionChar) -> numOfOccurrences, (insertionChar, pair._2) -> numOfOccurrences)
              }).getOrElse(Seq(pair -> numOfOccurrences))
        }).groupBy(_._1)
          .view
          .mapValues(_.map(_._2).sum)
          .toMap

        calculateOccurrencesForPairsAfterIterations(pairsOccurrencesAfterOneIteration, iterationsLeft - 1)
      }
    }

    val pairs = (0 until template.length-1).map(i => (template(i), template(i+1)))
      .groupBy(identity)
      .view
      .mapValues(_.size.toLong)
      .toMap

    val charToNumOfOccurrencesInPairs = calculateOccurrencesForPairsAfterIterations(pairs, iterations).toSeq.flatMap({
      case ((start, end), num) => Seq(start -> num, end -> num)
    })

    charToNumOfOccurrencesInPairs.groupBy(_._1).map({
      case (c, occurrences) =>
        val sumOfOccurrences = occurrences.map(_._2).sum
        (c, (sumOfOccurrences + sumOfOccurrences % 2)/2)
    })
  }

  def getPolymerFrequencyDiff(charFrequency: Map[Char, Long]): Long = {
    charFrequency.values.max - charFrequency.values.min
  }

  def parseInput(isTest: Boolean = false): (String, Map[(Char, Char), Char]) = {
    val lines = readFileLines[String](14, isTest = isTest)

    val template = lines.head
    val insertionRules = lines.filter(_.contains("->"))
      .map(line => {
        val Array(from, to) = line.split("->").map(_.trim)
        ((from.head, from(1)), to.head)
      }).toMap

    (template, insertionRules)
  }

  def main(args: Array[String]): Unit = {
    val (templateTest, rulesTest) = parseInput(isTest = true)

    val testPolymer10 = producePolymer(templateTest, rulesTest, 10)
    val testPolymer40 = producePolymer(templateTest, rulesTest, 40)
    println(getPolymerFrequencyDiff(testPolymer10))
    println(getPolymerFrequencyDiff(testPolymer40))

    val (template, rules) = parseInput()

    val polymer10 = producePolymer(template, rules, 10)
    val polymer40 = producePolymer(template, rules, 40)
    println(getPolymerFrequencyDiff(polymer10))
    println(getPolymerFrequencyDiff(polymer40))
  }
}
