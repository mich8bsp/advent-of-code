package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day5 {
  private def findSegmentsIntersection(
                                        firstStart: Int,
                                        firstEnd: Int,
                                        secondStart: Int,
                                        secondEnd: Int
                                      ): Option[(Int, Int)] = {
    if (firstStart > firstEnd) {
      return findSegmentsIntersection(firstEnd, firstStart, secondStart, secondEnd)
    }
    if (secondStart > secondEnd) {
      return findSegmentsIntersection(firstStart, firstEnd, secondEnd, secondStart)
    }
    if (firstStart > secondStart) {
      return findSegmentsIntersection(secondStart, secondEnd, firstStart, firstEnd)
    }
    if (firstEnd < secondStart) {
      None
    } else {
      Some(secondStart, math.min(firstEnd, secondEnd))
    }

  }

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point) {

    val A: Int = end.y - start.y
    val B: Int = start.x - end.x
    val C: Int = A * start.x + B * start.y

    val points: List[Point] = if (isHorizontal) {
      (math.min(start.x, end.x) to math.max(start.x, end.x)).map(x => Point(x, start.y)).toList
    } else if (isVertical) {
      (math.min(start.y, end.y) to math.max(start.y, end.y)).map(y => Point(start.x, y)).toList
    } else {
      val xRange = if (end.x >= start.x) start.x to end.x else start.x to end.x by -1
      val yRange = if (end.y >= start.y) start.y to end.y else start.y to end.y by -1
      xRange.zip(yRange).map({
        case (x, y) => Point(x, y)
      }).toList
    }

    def isHorizontal: Boolean = start.y == end.y

    def isVertical: Boolean = start.x == end.x

    def isDiagonal: Boolean = math.abs(start.x - end.x) == math.abs(start.y - end.y)

    def contains(point: Point): Boolean = this.points.contains(point)

    def intersect(other: Line): Option[Line] = {
        val A1 = this.A
        val A2 = other.A
        val B1 = this.B
        val B2 = other.B
        val C1 = this.C
        val C2 = other.C

        val det: Int = A1 * B2 - A2 * B1
        if (det == 0) {
          if (other.contains(this.start) || other.contains(this.end) || this.contains(other.start) || this.contains(other.end)) {
            if(isVertical && other.isVertical){
              findSegmentsIntersection(this.start.y, this.end.y, other.start.y, other.end.y)
                .map(intersection => Line(start = Point(this.start.x, intersection._1), end = Point(this.start.x, intersection._2)))
            }else{
              findSegmentsIntersection(this.start.x, this.end.x, other.start.x, other.end.x)
                .map(intersection => {
                  val x1 = intersection._1
                  val y1 = (C1 - A1 * x1) / B1
                  val x2 = intersection._2
                  val y2 = (C1 - A1 * x2) / B1
                  Line(start = Point(x1, y1), end = Point(x2, y2))
                })
            }
          } else {
            None
          }
        } else {
          val intersectionX = (B2 * C1 - B1 * C2) / det
          val intersectionY = (A1 * C2 - A2 * C1) / det
          val intersectionPoint: Point = Point(intersectionX, intersectionY)
          if (this.contains(intersectionPoint) && other.contains(intersectionPoint)) {
            Some(Line(intersectionPoint, intersectionPoint))
          } else {
            None
          }
        }

    }
  }

  def countOverlappingPoints(lines: List[Line]): Int = {
    val intersections: List[Line] = (for {
      i <- lines.indices
      j <- lines.indices if j > i
    } yield {
      lines(i).intersect(lines(j))
    }).flatten.toList

    val intersectionPoints: mutable.Set[Point] = mutable.Set[Point]()
    intersections.foreach(intersection => intersectionPoints.addAll(intersection.points))

    intersectionPoints.size

  }

  def main(args: Array[String]): Unit = {
    val lines: List[Line] = Source.fromResource("aoc2021/input_5.txt").getLines().toList
      .filter(_.nonEmpty)
      .map(x => {
        val Array(startStr, endStr) = x.split("->").map(_.trim)
        val Array(startX, startY) = startStr.split(",").map(_.trim).map(_.toInt)
        val Array(endX, endY) = endStr.split(",").map(_.trim).map(_.toInt)
        Line(
          start = Point(startX, startY),
          end = Point(endX, endY)
        )
      })

    val verticalOrHorizontalLines: List[Line] = lines.filter(line => line.isHorizontal || line.isVertical)
    println(countOverlappingPoints(verticalOrHorizontalLines))

    val verticalHorizontalOrDiagonalLines: List[Line] = lines.filter(line => line.isHorizontal || line.isVertical || line.isDiagonal)
    println(countOverlappingPoints(verticalHorizontalOrDiagonalLines))
  }
}
