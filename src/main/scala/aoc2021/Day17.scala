package aoc2021

import scala.collection.mutable

object Day17 {

  def parseInput(isTest: Boolean = false): (Range, Range) = {
    val line = readFileLines[String](17, isTest = isTest).head.split(":")(1).trim
    val x = line.split(",").head.split("=")(1).split("\\.\\.").map(_.toInt)
    val y = line.split(",")(1).split("=")(1).split("\\.\\.").map(_.toInt)

    (x.head to x(1), y.head to y(1))
  }

  def simulate(initialVel: (Int, Int), target: (Range, Range)): Option[Seq[(Int, Int)]] = {
    var currPos = (0, 0)
    var currVelX = initialVel._1
    var currVelY = initialVel._2
    val trajectory: mutable.Buffer[(Int, Int)] = mutable.Buffer[(Int, Int)]()
    while(currPos._2 >= target._2.start){
      trajectory.append(currPos)
      if(target._1.contains(currPos._1) && target._2.contains(currPos._2)){
        return Some(trajectory.toSeq)
      }
      currPos = (currPos._1 + currVelX, currPos._2 + currVelY)
      currVelX = if(currVelX > 0) currVelX - 1 else if (currVelX < 0) currVelX + 1 else currVelX
      currVelY = currVelY-1
    }
    None
  }

  def getMaxAmplitudeOfShotToTarget(target: (Range, Range)): Int = {
    (for {
      xVelocity <- 1 to target._1.end
      yVelocity <- target._2.start to -1*target._2.start
    } yield{
      simulate((xVelocity, yVelocity), target)
        .map(_.map(_._2).max)
    }).flatten.max
  }

  def getValidInitialVelocities(target: (Range, Range)): Seq[(Int, Int)] = {
   (for {
      xVelocity <- 1 to target._1.end
      yVelocity <- target._2.start to -1*target._2.start
    } yield{
      simulate((xVelocity, yVelocity), target)
        .map(_ => (xVelocity, yVelocity))
    }).flatten
  }

  def main(args: Array[String]): Unit = {
    val (xRangeTest, yRangeTest) = parseInput(isTest = true)
    println(getMaxAmplitudeOfShotToTarget((xRangeTest, yRangeTest))) //45

    val (xRange, yRange) = parseInput()
    println(getMaxAmplitudeOfShotToTarget((xRange, yRange))) //5671

    println(getValidInitialVelocities((xRangeTest, yRangeTest)).size) //112
    println(getValidInitialVelocities((xRange, yRange)).size) //4556
  }
}
