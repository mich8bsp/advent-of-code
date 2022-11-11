package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day19 {

  val possibleOrientations: Seq[Pos => Pos] = Seq(
    //  (x,y,z)
    (p: Pos) => p,
    //  (x,-y,-z)
    (p: Pos) => Pos(p.x, -p.y, -p.z),
    //  (-x,y,-z)
    (p: Pos) => Pos(-p.x, p.y, -p.z),
    //  (-x,-y,z)
    (p: Pos) => Pos(-p.x, -p.y, p.z),
    //  (x,z,-y)
    (p: Pos) => Pos(p.x, -p.z, p.y),
    //  (x,-z,y)
    (p: Pos) => Pos(p.x, p.z, -p.y),
    //  (-x,-z,-y)
    (p: Pos) => Pos(-p.x, -p.z, -p.y),
    //  (-x,z,y)
    (p: Pos) => Pos(-p.x, p.z, p.y),
    //  (y,-x,z)
    (p: Pos) => Pos(-p.y, p.x, p.z),
    //  (y,x,-z)
    (p: Pos) => Pos(p.y, p.x, -p.z),
    //  (y,z,x)
    (p: Pos) => Pos(p.z, p.x, p.y),
    //  (y,-z,-x)
    (p: Pos) => Pos(-p.z, p.x, -p.y),
    //  (-y,-x,-z)
    (p: Pos) => Pos(-p.y, -p.x, -p.z),
    //  (-y,x,z)
    (p: Pos) => Pos(p.y, -p.x, p.z),
    //  (-y,z,-x)
    (p: Pos) => Pos(-p.z, -p.x, p.y),
    //  (-y,-z,x)
    (p: Pos) => Pos(p.z, -p.x, -p.y),
    //  (z,x,y)
    (p: Pos) => Pos(p.y, p.z, p.x),
    //  (z,-x,-y)
    (p: Pos) => Pos(-p.y, -p.z, p.x),
    //  (z,y,-x)
      (p: Pos) => Pos(-p.z, p.y, p.x),
    //  (z,-y,x)
    (p: Pos) => Pos(p.z, -p.y, p.x),
    //  (-z,x,-y)
    (p: Pos) => Pos(p.y, -p.z, -p.x),
    //  (-z,-x,y)
    (p: Pos) => Pos(-p.y, p.z, -p.x),
    //  (-z,y,x)
    (p: Pos) => Pos(p.z, p.y, -p.x),
    //  (-z,-y,-x)
    (p: Pos) => Pos(-p.z, -p.y, -p.x)
  )

  case class Pos(x: Int, y: Int, z: Int) {
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y, z + other.z)
    def -(other: Pos): Pos = Pos(x - other.x, y - other.y, z - other.z)
    def *(other: Pos): Pos = Pos(x * other.x, y * other.y, z * other.z)

    def manhattanDistance(other: Pos): Int = {
      val diff = this - other
      math.abs(diff.x) + math.abs(diff.y) + math.abs(diff.z)
    }
  }

  class Scanner(val id: Int, val reports: List[Pos]){
    var origin: Option[Pos] = None
    var orientation: Option[Pos => Pos] = None

    def getAbsoluteCoordinateReports: List[Pos] = {
      reports.map(pos => {
        orientation.get.apply(pos) + origin.get
      })
    }

    def getRelativeCoordinateReports: List[Pos] = {
      reports.map(pos => {
        orientation.getOrElse((p: Pos) => p).apply(pos)
      })
    }
  }

  def getOrigin(positionsOfUnresolved: Set[Pos], positionsOfResolved: Set[Pos]): Option[Pos] = {
    for{
      positionOfUnresolved <- positionsOfUnresolved
      positionOfResolved <- positionsOfResolved
    }yield {
      //we assume it's the same position, so the difference is the origin
      val origin = positionOfResolved - positionOfUnresolved
      val resolutionWithSelectedOrigin = positionsOfUnresolved.map(_ + origin)
      if(resolutionWithSelectedOrigin.intersect(positionsOfResolved).size >= 12){
        return Some(origin)
      }
    }
    None
  }

  def resolvePositionAndOrientation(scanner: Scanner, resolvedScanners: List[Scanner]): Boolean = {
    val scannerIdToCoordinatesInResolved: Map[Int, Set[Pos]] = resolvedScanners.map(x => x.id -> x.getAbsoluteCoordinateReports.toSet).toMap

    for {
      orientation <- possibleOrientations
    }yield {
      scanner.orientation = Some(orientation)
      val relativePositionsForCurrent = scanner.getRelativeCoordinateReports.toSet
      var resolvedOriginForCurrent: Option[Pos] = None
      scannerIdToCoordinatesInResolved.values.foreach(resolvedToCompareTo => {
        if(resolvedOriginForCurrent.isEmpty){
          resolvedOriginForCurrent = getOrigin(relativePositionsForCurrent, resolvedToCompareTo)
        }
      })
      if(resolvedOriginForCurrent.isDefined){
        scanner.origin = resolvedOriginForCurrent
        return true
      }else{
        scanner.origin = None
        scanner.orientation = None
      }
    }
    false
  }

  def resolveScannerLocations(scanners: List[Scanner]): Unit = {
    while(scanners.exists(_.origin.isEmpty)) {
      val (resolvedScanners, unresolvedScanners) = scanners.partition(_.origin.isDefined)
      var resolvedChanged: Boolean = false
      unresolvedScanners.foreach(scanner => {
        if (!resolvedChanged && resolvePositionAndOrientation(scanner, resolvedScanners)) {
          resolvedChanged = true
        }
      })
    }
  }

  def getMaxManhattanDistance(scanners: List[Scanner]): Int = {
    val scannerById: Map[Int, Scanner] = scanners.map(s => s.id -> s).toMap
    (for {
      firstId <- scannerById.keySet
      first = scannerById(firstId)
      secondId <- scannerById.keySet.filter(_ > firstId)
      second = scannerById(secondId)
    }yield{
      first.origin.get.manhattanDistance(second.origin.get)
    }).max
  }

  def parseScanner(lines: List[String]): Scanner = {
    val id = lines.head.filter(_.isDigit).toInt
    val reports = lines.tail.map(line => {
      val Array(x,y,z) = line.split(",")
      Pos(x.toInt, y.toInt, z.toInt)
    })

    new Scanner(id, reports)
  }

  def parseInput(filePath: String): List[Scanner] = {
    val lines = Source.fromResource(filePath).getLines().toList

    var leftToSplit = lines
    val scannersParsed: mutable.Buffer[Scanner] = mutable.Buffer[Scanner]()
    while(leftToSplit.exists(_.trim.isEmpty)){
      val (currScannerLines, otherScannerLines) = leftToSplit.splitAt(leftToSplit.indexWhere(_.trim.isEmpty))
      leftToSplit = otherScannerLines.tail
      scannersParsed.append(parseScanner(currScannerLines))
    }
    scannersParsed.append(parseScanner(leftToSplit))

    scannersParsed.find(_.id == 0).foreach(referenceScanner => {
      referenceScanner.origin = Some(Pos(0,0,0))
      referenceScanner.orientation = Some((p: Pos) => p)
    })

    scannersParsed.toList
  }

  def main(args: Array[String]): Unit = {
    val scannersTest: List[Scanner] = parseInput("aoc2021/input_19_test.txt")
    resolveScannerLocations(scannersTest)
    println(scannersTest.flatMap(_.getAbsoluteCoordinateReports).toSet.size)
    println(getMaxManhattanDistance(scannersTest))

    val scanners: List[Scanner] = parseInput("aoc2021/input_19.txt")
    resolveScannerLocations(scanners)
    println(scanners.flatMap(_.getAbsoluteCoordinateReports).toSet.size)
    println(getMaxManhattanDistance(scanners))

  }
}
