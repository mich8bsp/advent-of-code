package aoc2021

object Day13 {

  def foldOrigami(points: Set[(Int, Int)], operations: List[(String, Int)]): Set[(Int, Int)] = {
    var pointsOnFolded = points
    operations.foreach({
      case ("x", col) =>
        pointsOnFolded = pointsOnFolded.map({
          case (x, y) => if(x > col){
            (col - (x-col), y)
          }else{
            (x, y)
          }
        })
      case ("y", row) =>
        pointsOnFolded = pointsOnFolded.map({
          case (x, y) => if(y > row){
            (x, row - (y-row))
          }else{
            (x, y)
          }
        })
    })

    pointsOnFolded
  }

  private def parseInput(isTest: Boolean = false): (Set[(Int, Int)], List[(String, Int)]) = {
    val lines = readFileLines[String](13, isTest = isTest)
    val points = lines.filter(_.nonEmpty).filterNot(_.contains("fold along"))
      .map(line => {
        val Array(x,y) = line.split(",")
        (x.toInt, y.toInt)
      })
    val operations = lines.filter(_.contains("fold along"))
      .map(line => {
        val Array(axis, num) = line.stripPrefix("fold along ").split("=")
        (axis, num.toInt)
      })

    (points.toSet, operations)
  }

  private def printOrigami(points: Set[(Int, Int)]): Unit = {
    val rows = points.map(_._2).max
    val cols = points.map(_._1).max

    val finalPaper = (0 to rows).map(r => {
      (0 to cols).map(c => {
        if(points.contains((c, r))){
          '#'
        }else{
          '.'
        }
      }).toArray
    }).toArray

    println(finalPaper.map(_.mkString("Array(", ", ", ")")).mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    val (pointsTest, operationsTest) = parseInput(isTest = true)

    println(foldOrigami(pointsTest, operationsTest.take(1)).size)

    val (points, operations) = parseInput()

    println(foldOrigami(points, operations.take(1)).size)

    val folded = foldOrigami(points, operations)

    printOrigami(folded)

  }
}
