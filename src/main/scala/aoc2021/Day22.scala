package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day22 {

  def parseInput(filePath: String): List[((Int, Int), (Int, Int), (Int, Int), Boolean)] = {
    def parseRange(rangeStr: String): (Int, Int) = {
      val cleanRangeSplit = rangeStr.split("=")(1).split("\\.\\.")
      (cleanRangeSplit.head.toInt, cleanRangeSplit(1).toInt)
    }

    Source.fromResource(filePath).getLines().toList
      .map(line => {
        val Array(isOn, range) = line.split(" ")
        val xRange = parseRange(range.split(",").head)
        val yRange = parseRange(range.split(",")(1))
        val zRange = parseRange(range.split(",")(2))

        (xRange, yRange, zRange, isOn == "on")
      })
  }

  case class Cube(xRange: (Int, Int), yRange: (Int, Int), zRange: (Int, Int)){

    def intersect(other: Cube): Option[Cube] = {
      for{
        xIntersection <- findSegmentsIntersection(this.xRange, other.xRange)
        yIntersection <- findSegmentsIntersection(this.yRange, other.yRange)
        zIntersection <- findSegmentsIntersection(this.zRange, other.zRange)
      }yield{
        Cube(xRange = xIntersection, yRange = yIntersection, zRange = zIntersection)
      }
    }

    def size: Long = (xRange._2 - xRange._1 + 1).toLong * (yRange._2 - yRange._1 + 1).toLong * (zRange._2 - zRange._1 + 1).toLong
  }

  def findSegmentsIntersection(first: (Int, Int), second: (Int, Int)): Option[(Int, Int)] = {
    if (first._1 > second._1) {
      return findSegmentsIntersection(second, first)
    }
    if (first._2 < second._1) {
      None
    } else {
      Some((second._1, math.min(first._2, second._2)))
    }
  }

  def countOnCubesAfterSteps(steps: List[((Int, Int), (Int, Int), (Int, Int), Boolean)]): Long = {
    var cubesOn: List[Cube] = List()
    steps.foreach(step => {
      val isStepTurningOn: Boolean = step._4
      if(cubesOn.isEmpty){
        if(isStepTurningOn){
          cubesOn = List(Cube(step._1, step._2, step._3))
        }
      }else{
        var stepCubes = List(Cube(step._1, step._2, step._3))
        cubesOn = cubesOn.flatMap(cube => {
          val (resultOnCubes, leftStepCubes) = intersect(cube, stepCubes, isStepTurningOn)
          stepCubes = leftStepCubes
          resultOnCubes
        })

        if(isStepTurningOn){
          cubesOn = cubesOn ++ stepCubes
        }
      }
    })

    cubesOn.map(_.size).sum
  }

  def intersect(onCube: Cube, stepCubes: List[Cube], stepCubesOn: Boolean): (List[Cube], List[Cube]) = {
    if(!stepCubesOn){
      var leftOnCubes = List(onCube)
      stepCubes.foreach(cubeToSwitchOff => {
        leftOnCubes = leftOnCubes.flatMap(cubeOn => diff(cubeOn, cubeToSwitchOff))
      })

      (leftOnCubes, stepCubes)
    }else{
      val leftStepCubes = mutable.Buffer[Cube]()
      stepCubes.foreach(stepCube => {
        leftStepCubes.addAll(diff(stepCube, onCube))
      })

      (List(onCube), leftStepCubes.toList)
    }
  }

  def diff(diffFrom: Cube, diffWith: Cube): List[Cube] = {
    val intersectionCubeOpt = diffFrom.intersect(diffWith)
    intersectionCubeOpt match {
      case None => List(diffFrom)
      case Some(x) if x == diffFrom => List[Cube]()
      case Some(intersectionCube) =>
        val xSections = splitRangeByIntersection(diffFrom.xRange, intersectionCube.xRange)
        val ySections = splitRangeByIntersection(diffFrom.yRange, intersectionCube.yRange)
        val zSections = splitRangeByIntersection(diffFrom.zRange, intersectionCube.zRange)

        (for {
          xSection <- xSections
          ySection <- ySections
          zSection <- zSections if Cube(xSection, ySection, zSection) != intersectionCube
        }yield {
          Cube(xSection, ySection, zSection)
        }).toList
    }
  }

  def splitRangeByIntersection(range: (Int, Int), intersection: (Int, Int)): Seq[(Int, Int)] = {
    Seq[(Int, Int)](
      (range._1, intersection._1 - 1),
      intersection,
      (intersection._2+1, range._2)
    ).filter(r => r._1<=r._2)
  }


  def main(args: Array[String]): Unit = {
    val stepsTest = parseInput("aoc2021/input_22_test.txt")
    val initializationArea = (-50, 50)

    val stepsTestInInitialization = stepsTest.filter(x =>
      findSegmentsIntersection(x._1, initializationArea).nonEmpty &&
        findSegmentsIntersection(x._2, initializationArea).nonEmpty &&
        findSegmentsIntersection(x._3, initializationArea).nonEmpty)

    println(countOnCubesAfterSteps(stepsTestInInitialization)) //474140
    println(countOnCubesAfterSteps(stepsTest)) //2758514936282235

    val steps = parseInput("aoc2021/input_22.txt")

    val stepsInInitialization = steps.filter(x =>
      findSegmentsIntersection(x._1, initializationArea).nonEmpty &&
        findSegmentsIntersection(x._2, initializationArea).nonEmpty &&
        findSegmentsIntersection(x._3, initializationArea).nonEmpty)

    println(countOnCubesAfterSteps(stepsInInitialization)) //503864
    println(countOnCubesAfterSteps(steps)) //1255547543528356

  }
}
