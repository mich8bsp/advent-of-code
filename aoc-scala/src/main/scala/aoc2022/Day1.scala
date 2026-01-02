package aoc2022

object Day1 {

  case class Bag(itemCalories: List[Int]) {
    lazy val bagCalories: Int = itemCalories.sum
  }

  def solveA(bags: Seq[Bag]): Int = {
    bags.map(_.bagCalories)
      .max
  }

  def solveB(bags: Seq[Bag]): Int = {
    bags.map(_.bagCalories)
      .sorted(Ordering[Int].reverse)
      .take(3)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val bags: Seq[Bag] = readSections[Bag](1) { lines => {
      Bag(lines.map(_.toInt))
    }}

    println(solveA(bags))
    println(solveB(bags))
  }
}
