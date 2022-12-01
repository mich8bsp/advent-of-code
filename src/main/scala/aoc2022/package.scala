import Utils._

package object aoc2022 {
  val YEAR = 2022
  def readFileLines[T](day: Int, isTest: Boolean = false)
                      (implicit parser: LineParser[T]): List[T] = {
    Utils.readFileLines[T](YEAR, day, isTest)
  }

  def readCharGrid(day: Int, isTest: Boolean = false): Array[Array[Char]] = {
    Utils.readCharGrid(YEAR, day, isTest)
  }

  def readDigitsGrid(day: Int, isTest: Boolean = false): Array[Array[Int]] = {
    Utils.readDigitsGrid(YEAR, day, isTest)
  }

  def readSections[T](day: Int, isTest: Boolean = false)
                     (implicit parser: SectionParser[T]): List[T] = {
    Utils.readSections[T](YEAR, day, isTest)
  }
}