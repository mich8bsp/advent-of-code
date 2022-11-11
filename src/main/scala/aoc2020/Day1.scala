package aoc2020

object Day1 {

  def solveA(expenses: List[Int]): Option[Long] = {
    val targetSum = 2020
    val validExpenses = expenses.filter(_ < targetSum)
    val expensesByCount: Map[Int, Int] = validExpenses.groupMapReduce(identity)(_ => 1)(_ + _)
    (for {
      (first, firstCount) <- expensesByCount
      second = targetSum - first
      if expensesByCount.contains(second)
    } yield {
      val isValidSum = (first, second) match {
        case (`first`, `first`) => firstCount >= 2
        case _ => true
      }

      Some(first.toLong * second.toLong).filter(_ => isValidSum)
    }).flatten
      .headOption
  }

  def solveB(expenses: List[Int]): Option[Long] = {
    val targetSum = 2020
    val validExpenses = expenses.filter(_ < targetSum)
    val expensesByCount: Map[Int, Int] = validExpenses.groupMapReduce(identity)(_ => 1)(_ + _)
    (for {
      (first, firstCount) <- expensesByCount
      (second, secondCount) <- expensesByCount
      third = targetSum - first - second
      if expensesByCount.contains(third)
    } yield {
      val isValidSum = (first, second, third) match {
        case (`first`, `first`, `first`) => firstCount >= 3
        case (`first`, `first`, _) | (`first`, _, `first`) => firstCount >= 2
        case (_, `second`, `second`) => secondCount >= 2
        case _ => true
      }

      Some(first.toLong * second.toLong * third.toLong).filter(_ => isValidSum)
    }).flatten
      .headOption
  }

  def main(args: Array[String]): Unit = {
    val expenses: List[Int] = readFileLines[Int](1)
    println(solveA(expenses))
    println(solveB(expenses))
  }
}
