package aoc2021

import scala.collection.mutable
import scala.util.Try

object Day10 {

  private def reverse(c: Char): Char = c match {
    case '(' => ')'
    case '{' => '}'
    case '<' => '>'
    case ')' => '('
    case '}' => '{'
    case '>' => '<'
    case '[' => ']'
    case ']' => '['
  }

  private def getValueOf(c: Char): Int = c match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  def getLineSyntaxErrorScore(line: String): Int = {
    val openStack: mutable.Stack[Char] = mutable.Stack[Char]()

    line.toCharArray.foreach {
      case c@('(' | '{' | '<' | '[') => openStack.push(c)
      case c@('}' | ')' | '>' | ']') =>
        if(!Try(openStack.pop()).toOption.contains(reverse(c))){
          return getValueOf(c)
        }
    }

    0
  }

  def getSyntaxErrorScore(lines: List[String]): Int = {
    lines.map(getLineSyntaxErrorScore).sum
  }

  def getAutocompleteScore(line: String): Long = {
    val openStack: mutable.Stack[Char] = mutable.Stack[Char]()

    line.toCharArray.foreach {
      case c@('(' | '{' | '<' | '[') => openStack.push(c)
      case '}' | ')' | '>' | ']' => openStack.pop()
    }

    var score: Long = 0L
    while(openStack.nonEmpty){
      val braceToClose = openStack.pop()
      braceToClose match {
        case '(' => score = score*5 + 1
        case '[' => score = score*5 + 2
        case '{' => score = score*5 + 3
        case '<' => score = score*5 + 4
      }
    }

    score
  }

  def getMiddleAutocompleteScore(lines: List[String]): Long = {
    val nonCorruptedLines: List[String] = lines.filter(line => getLineSyntaxErrorScore(line) == 0)
    val autocompleteScores: List[Long] = nonCorruptedLines.map(getAutocompleteScore)
    autocompleteScores.sorted.apply(autocompleteScores.size / 2)
  }

  def main(args: Array[String]): Unit = {
    val testLines = readFileLines[String](10, isTest = true)
    println(getSyntaxErrorScore(testLines))

    val lines = readFileLines[String](10)

    println(getSyntaxErrorScore(lines))

    println(getMiddleAutocompleteScore(testLines))
    println(getMiddleAutocompleteScore(lines))
  }
}
