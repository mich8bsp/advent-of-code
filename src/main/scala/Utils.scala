import scala.collection.mutable
import scala.io.Source

object Utils {
  trait LineParser[T] {
    def parse(line: String): T
  }

  implicit object IntParser extends LineParser[Int] {
    override def parse(line: String): Int = line.toInt
  }

  implicit object DoubleParser extends LineParser[Double] {
    override def parse(line: String): Double = line.toDouble
  }

  implicit object StringParser extends LineParser[String] {
    override def parse(line: String): String = line
  }

  implicit object ListIntParser extends LineParser[List[Int]] {
    override def parse(line: String): List[Int] = line.split(",").map(IntParser.parse).toList
  }

  implicit object ListDoubleParser extends LineParser[List[Double]] {
    override def parse(line: String): List[Double] = line.split(",").map(DoubleParser.parse).toList
  }

  implicit object ListStringParser extends LineParser[List[String]] {
    override def parse(line: String): List[String] = line.split(",").toList
  }

  trait SectionParser[T] {
    def parseSection(lines: List[String]): T
  }

  def readFileLines[T](year: Int, day: Int, isTest: Boolean = false)
                      (implicit parser: LineParser[T]): List[T] = {
    Source.fromResource(s"aoc$year/input_$day${if (isTest) "_test" else ""}.txt")
      .getLines()
      .map(parser.parse)
      .toList
  }

  def readCharGrid(year: Int, day: Int, isTest: Boolean = false): Array[Array[Char]] = {
    Source.fromResource(s"aoc$year/input_$day${if (isTest) "_test" else ""}.txt")
      .getLines()
      .map(_.toCharArray)
      .toArray
  }

  def readDigitsGrid(year: Int, day: Int, isTest: Boolean = false): Array[Array[Int]] = {
    readCharGrid(year, day, isTest)
      .map(_.map(_.asDigit))
  }

  def readSections[T](year: Int, day: Int, isTest: Boolean = false)
                     (implicit parser: SectionParser[T]): List[T] = {
    val sections = mutable.Buffer[T]()
    var linesLeft = readFileLines[String](year, day, isTest)
    while (linesLeft.nonEmpty) {
      val currSectionLines = linesLeft.takeWhile(_.trim.nonEmpty)
      sections.append(parser.parseSection(currSectionLines))
      linesLeft = linesLeft.dropWhile(_.trim.nonEmpty)
      if (linesLeft.nonEmpty) {
        linesLeft = linesLeft.tail
      }
    }

    sections.toList
  }
}
