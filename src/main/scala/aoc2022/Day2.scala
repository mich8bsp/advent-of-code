package aoc2022

object Day2 {

  object GameShape extends Enumeration {
    val Rock, Paper, Scissors = Value
  }

  object GameOutcome extends Enumeration {
    val Win, Lose, Draw = Value
  }

  private def parseOutcome(str: String): GameOutcome.Value = str match {
    case "X" => GameOutcome.Lose
    case "Y" => GameOutcome.Draw
    case "Z" => GameOutcome.Win
  }

  private def parseGameShape(str: String): GameShape.Value = str match {
    case "A" | "X" => GameShape.Rock
    case "B" | "Y" => GameShape.Paper
    case "C" | "Z" => GameShape.Scissors
  }

  private def getLosingHand(opponentHand: GameShape.Value): GameShape.Value = opponentHand match {
    case GameShape.Rock => GameShape.Scissors
    case GameShape.Paper => GameShape.Rock
    case GameShape.Scissors => GameShape.Paper
  }

  private def getWinningHand(opponentHand: GameShape.Value): GameShape.Value = opponentHand match {
    case GameShape.Rock => GameShape.Paper
    case GameShape.Paper => GameShape.Scissors
    case GameShape.Scissors => GameShape.Rock
  }

  def solveA(game: List[(GameShape.Value, GameShape.Value)]): Int = {
    def evaluateRound(opponentHand: GameShape.Value, playerHand: GameShape.Value): Int = {
      val outcome: GameOutcome.Value = (opponentHand, playerHand) match {
        case (x, y) if x == y => GameOutcome.Draw
        case (x, y) if y == getWinningHand(x) => GameOutcome.Win
        case _ => GameOutcome.Lose
      }

      val shapeScore: Int = playerHand match {
        case GameShape.Rock => 1
        case GameShape.Paper => 2
        case GameShape.Scissors => 3
      }

      val outcomeScore: Int = outcome match {
        case GameOutcome.Win => 6
        case GameOutcome.Draw => 3
        case GameOutcome.Lose => 0
      }

      shapeScore + outcomeScore
    }

    game.map((evaluateRound _).tupled).sum
  }

  def solveB(game: List[(GameShape.Value, GameOutcome.Value)]): Int = {
    solveA(game.map {
      case (opponentHand, GameOutcome.Lose) => (opponentHand, getLosingHand(opponentHand))
      case (opponentHand, GameOutcome.Win) => (opponentHand, getWinningHand(opponentHand))
      case (opponentHand, GameOutcome.Draw) => (opponentHand, opponentHand)
    })
  }

  def main(args: Array[String]): Unit = {
    val gameA = readFileLines[(GameShape.Value, GameShape.Value)](2) { line =>
     val Array(opponentHand, playerHand) = line.split(" ").map(parseGameShape)

      (opponentHand, playerHand)
    }

    val gameB = readFileLines[(GameShape.Value, GameOutcome.Value)](2) { line =>
      val Array(opponentHandStr, outcomeStr) = line.split(" ")

      (parseGameShape(opponentHandStr), parseOutcome(outcomeStr))
    }

    println(solveA(gameA))
    println(solveB(gameB))
  }
}
