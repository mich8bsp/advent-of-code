package aoc2020

object Day5 {

  val FlightRows = 128
  val FlightCols = 8

  def findSeatNumber(boardingPass: String): (Int, Int) = {
    boardingPass.foldLeft((0 until FlightRows, 0 until FlightCols)) {
      case ((rowRange, colRange), 'F') => (rowRange.start until (rowRange.start + (rowRange.end - rowRange.start) / 2), colRange)
      case ((rowRange, colRange), 'B') => ((rowRange.start + (rowRange.end - rowRange.start) / 2) until rowRange.end, colRange)
      case ((rowRange, colRange), 'L') => (rowRange, colRange.start until (colRange.start + (colRange.end - colRange.start) / 2))
      case ((rowRange, colRange), 'R') => (rowRange, (colRange.start + (colRange.end - colRange.start) / 2) until colRange.end)
    } match {
      case (resRowRange, resColRange) =>
        if (resRowRange.size != 1 || resColRange.size != 1) {
          throw new Exception(s"Invalid boarding pass: $boardingPass")
        }
        (resRowRange.start, resColRange.start)
    }
  }

  def solveA(passes: List[String]): Int = {
    passes.map(findSeatNumber)
      .map {
        case (row, col) => row * FlightCols + col
      }.max
  }

  def solveB(passes: List[String]): Int = {
    val rowsNotFullyOccupied = passes.map(findSeatNumber)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .filter {
        case (_, seats) => seats.size < FlightCols
      }

    val row = rowsNotFullyOccupied.keys.toList.sorted.drop(1).dropRight(1).head
    val col = (0 until FlightCols).sum - rowsNotFullyOccupied(row).sum
    row * FlightCols + col
  }

  def main(args: Array[String]): Unit = {
    val boardingPasses = readFileLines[String](5)
    println(solveA(boardingPasses))
    println(solveB(boardingPasses))
  }
}
