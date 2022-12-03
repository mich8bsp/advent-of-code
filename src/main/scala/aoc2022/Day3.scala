package aoc2022

object Day3 {

  private def findSharedItemTypeScore(itemList: List[Set[Char]]): Int = {
    val sharedItemType: Char = itemList.reduce(_ intersect _).head
    if (sharedItemType.isUpper) {
      sharedItemType - 'A' + 27
    } else {
      sharedItemType - 'a' + 1
    }
  }

  def solveA(rucksacks: List[String]): Int = {
    rucksacks.map { r =>
      val (compartment1, compartment2) = r.splitAt(r.length / 2)
      findSharedItemTypeScore(List(compartment1.toSet, compartment2.toSet))
    }.sum
  }

  def solveB(rucksacks: List[String]): Int = {
    rucksacks.grouped(3)
      .map(_.map(_.toSet))
      .map(findSharedItemTypeScore)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val rucksacks: List[String] = readFileLines[String](3)
    println(solveA(rucksacks))
    println(solveB(rucksacks))
  }
}
