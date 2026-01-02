package aoc2025

object Day1 {
  object Direction extends Enumeration {
    val Left, Right = Value
  }

  case class Rotation(dir: Direction.Value, steps: Int)

  def solve(startPos: Int, rotations: Seq[Rotation]): Int = {
    val (finalPos, finalCode) = rotations.foldLeft((startPos, 0)) { case ((currPos, currCode), currRotation) =>
      val newPos = currRotation match {
        case Rotation(Direction.Left, steps) => (currPos - steps) % 100
        case Rotation(Direction.Right, steps) => (currPos + steps) % 100
      }
      val newCode = if (newPos == 0) {
        currCode + 1
      } else {
        currCode
      }
      (newPos, newCode)
    }

    finalCode
  }

  def solve2(startPos: Int, rotations: Seq[Rotation]): Int = {
    val (finalPos, finalCode) = rotations.foldLeft((startPos, 0)) { case ((currPos, currCode), currRotation) =>
      val newPos = currRotation match {
        case Rotation(Direction.Left, steps) => currPos - steps
        case Rotation(Direction.Right, steps) => currPos + steps
      }

      val newCode = currCode + (math.floorDiv(math.max(currPos, newPos), 100) - math.floorDiv(math.min(currPos, newPos) - 1, 100))
      if (currPos % 100 == 0 ) {
        (newPos, newCode - 1)
      } else {
        (newPos, newCode)
      }
    }

    finalCode
  }


  private def parseDirections(): Seq[Rotation] = {
    readFileLines(1) { line =>
      val direction = line.head match {
        case 'L' => Direction.Left
        case 'R' => Direction.Right
      }
      val steps = line.drop(1).toInt
      Rotation(direction, steps)
    }
  }

  def main(args: Array[String]): Unit = {
    val rotations = parseDirections()
    val startPos: Int = 50
    val code: Int = solve(startPos, rotations)
    println(code)

    val code2: Int = solve2(startPos, rotations)
    println(code2)
  }

}
