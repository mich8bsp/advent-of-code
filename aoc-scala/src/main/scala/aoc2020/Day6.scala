package aoc2020

object Day6 {

  def parseForms: List[List[Set[Char]]] = {
    readSections[List[Set[Char]]](6) { formLines =>
      formLines.map(_.toCharArray.toSet)
    }
  }

  def solveA(forms: List[List[Set[Char]]]): Int = {
    forms.map(_.reduce(_ ++ _))
      .map(_.size)
      .sum
  }

  def solveB(forms: List[List[Set[Char]]]): Int = {
    forms.map(_.reduce(_ intersect _))
      .map(_.size)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val forms: List[List[Set[Char]]] = parseForms
    println(solveA(forms))
    println(solveB(forms))
  }
}
