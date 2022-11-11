package aoc2020

object Day2 {

  def isValidPasswordOldPolicy(rule: PasswordRule, password: String): Boolean = {
    rule.numOfTimes.contains(password.count(_ == rule.charToAppear))
  }

  def isValidPasswordNewPolicy(rule: PasswordRule, password: String): Boolean = {
    val targetChar = rule.charToAppear

    (password(rule.numOfTimes.start - 1), password(rule.numOfTimes.end - 1)) match {
      case (`targetChar`, `targetChar`) => false
      case (`targetChar`, _) => true
      case (_, `targetChar`) => true
      case _ => false
    }
  }

  def solveA(passwords: List[(PasswordRule, String)]): Int = {
    passwords.count((isValidPasswordOldPolicy _).tupled)
  }

  def solveB(passwords: List[(PasswordRule, String)]): Int = {
    passwords.count((isValidPasswordNewPolicy _).tupled)
  }

  case class PasswordRule(charToAppear: Char, numOfTimes: Range)

  private def parsePassword(line: String): (PasswordRule, String) = {
    val Array(ruleRaw, passwordRaw) = line.split(":")
    val Array(rangeStr, charToAppear) = ruleRaw.split(" ")
    val Array(lowStr, highStr) = rangeStr.split("-")
    (PasswordRule(charToAppear.head, lowStr.toInt to highStr.toInt), passwordRaw.trim)
  }

  def main(args: Array[String]): Unit = {
    val passwords = readFileLines[(PasswordRule, String)](2)(parsePassword)
    println(solveA(passwords))
    println(solveB(passwords))
  }
}
