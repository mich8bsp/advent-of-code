package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day23 {

  class Board(arr: Array[Array[Char]]) {
    val posCantStopOn = Seq((1, 3), (1, 5), (1, 7), (1, 9))
    val destinationByAmphipod: Map[Char, Seq[(Int, Int)]] = Map(
      'A' -> (2 until arr.length - 1).map(row => (row, 3)),
      'B' -> (2 until arr.length - 1).map(row => (row, 5)),
      'C' -> (2 until arr.length - 1).map(row => (row, 7)),
      'D' -> (2 until arr.length - 1).map(row => (row, 9))
    )

    def isInHallway(pos: (Int, Int)): Boolean = {
      pos._1 == 1
    }

    def isOrganized(amphipodPositions: Map[(Int, Int), Char]): Boolean = amphipodPositions.forall({
      case (pos, amphipod) => destinationByAmphipod(amphipod).contains(pos)
    })

    def isOccupied(pos: (Int, Int), amphipodLocations: Map[(Int, Int), Char]): Boolean = {
      amphipodLocations.contains(pos) || arr(pos._1)(pos._2) == '#'
    }

    def shouldAmphipodMove(pos: (Int, Int), amphipod: Char, allAmphipodPositions: Map[(Int, Int), Char]): Boolean = {
      val isNotInCorrectRoom = !destinationByAmphipod(amphipod).contains(pos)
      val isBlockingAmphipodsNotInPlace = destinationByAmphipod(amphipod).exists({
          case (i, j) => i > pos._1 && allAmphipodPositions.get((i, j)).exists(_ != amphipod)
        })

      isNotInCorrectRoom || isBlockingAmphipodsNotInPlace
    }

    def getPossibleDestinations(startPosition: (Int, Int), currAmphipod: Char, allAmphipods: Map[(Int, Int), Char]): Seq[((Int, Int), Int)] = {
      val visited: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
      val dfsStack: mutable.Stack[((Int, Int), Int)] = mutable.Stack[((Int, Int), Int)]()
      dfsStack.addOne((startPosition, 0))
      visited.add(startPosition)

      val destinations: mutable.Set[((Int, Int), Int)] = mutable.Set[((Int, Int), Int)]()

      while (dfsStack.nonEmpty) {
        val topStack = dfsStack.pop()
        val (i, j) = topStack._1
        val currentSteps = topStack._2
        val validNeighbors = Seq((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
          .filter(neighbor => !isOccupied(neighbor, allAmphipods))
          .filter(neighbor => !visited.contains(neighbor))

        visited.addAll(validNeighbors)
        dfsStack.addAll(validNeighbors.map(x => (x, currentSteps + 1)))
        destinations.addAll(validNeighbors.map(x => (x, currentSteps + 1)))
      }

      destinations.filter(dest => {
        val destPosition = dest._1
        if (isInHallway(destPosition)) {
          !posCantStopOn.contains(destPosition) && //can't stop just outside a room
            !isInHallway(startPosition) //can't go hallway to hallway
        } else {
          val inCorrectRoom: Boolean = destinationByAmphipod(currAmphipod).contains(destPosition)
          val isFarthestAndNotBlocking: Boolean = destinationByAmphipod(currAmphipod).forall({
            case (i,j) => if(i > destPosition._1){
              allAmphipods.get((i,j)).contains(currAmphipod)
            }else{
              true
            }
          })

          inCorrectRoom && isFarthestAndNotBlocking
        }
      }).toSeq
    }
  }

  def calcEnergyCost(amphipod: Char, steps: Int): Long = {
    (amphipod match {
      case 'A' => 1
      case 'B' => 10
      case 'C' => 100
      case 'D' => 1000
    }) * steps
  }

  val cache = mutable.Map[Map[(Int, Int), Char], Long]()

  def organizeAmphipods(board: Board, amphipodPositions: Map[(Int, Int), Char]): Long = {
    cache.getOrElseUpdate(amphipodPositions, {
      if (board.isOrganized(amphipodPositions)) {
        0L
      } else {
        val possibleResults = amphipodPositions.flatMap({
          case (pos, amphipod) =>
            if (!board.shouldAmphipodMove(pos, amphipod, amphipodPositions)) {
              None
            } else {
              val destinations = board.getPossibleDestinations(pos, amphipod, amphipodPositions)
              if (destinations.isEmpty) {
                None
              } else {
                Some(destinations.map({ case (destPosition, moveSteps) =>
                  calcEnergyCost(amphipod, moveSteps) + organizeAmphipods(board, amphipodPositions.-(pos).+(destPosition -> amphipod))
                }).min)
              }
            }
        })

        if (possibleResults.isEmpty) {
          Int.MaxValue
        } else {
          possibleResults.min
        }
      }
    })
  }

  def parseInput(filePath: String): (Board, Map[(Int, Int), Char]) = {
    val lines = Source.fromResource(filePath).getLines().toList
    parseInput(lines)
  }

  def parseInputUnfolded(filePath: String): (Board, Map[(Int, Int), Char]) = {
    val lines = Source.fromResource(filePath).getLines().toList
    val unfoldedLines = lines.take(3) ++ List(
      "  #D#C#B#A#  ",
      "  #D#B#A#C#  "
    ) ++ lines.drop(3)

    parseInput(unfoldedLines)
  }

  def parseInput(lines: List[String]): (Board, Map[(Int, Int), Char]) = {
    val arr = lines.toArray.map(_.toCharArray)
    val board = new Board(arr.map(_.map(c => if (c.isLetter) '.' else c)))
    val amphipodPositions = (for {
      i <- arr.indices
      j <- arr(i).indices
    } yield {
      if (arr(i)(j).isLetter) {
        Some((i, j) -> arr(i)(j))
      } else {
        None
      }
    }).flatten.toMap

    (board, amphipodPositions)
  }

  def main(args: Array[String]): Unit = {
    val (boardTest, initPositionsTest) = parseInput("aoc2021/input_23_test.txt")

    println(organizeAmphipods(boardTest, initPositionsTest))

    val (board, initPositions) = parseInput("aoc2021/input_23.txt")

    println(organizeAmphipods(board, initPositions))

    val (boardTestUnfolded, initPositionsTestUnfolded) = parseInputUnfolded("aoc2021/input_23_test.txt")

    println(organizeAmphipods(boardTestUnfolded, initPositionsTestUnfolded))

    val (boardUnfolded, initPositionsUnfolded) = parseInputUnfolded("aoc2021/input_23.txt")

    println(organizeAmphipods(boardUnfolded, initPositionsUnfolded))
  }
}
