package aoc2021

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day4 {

  class Board(val field: Array[Array[Int]]) {
    def isBingo: Boolean = {
      lazy val isRowBingo: Boolean = field.exists(_.forall(_ < 0))
      lazy val isColBingo: Boolean = field.indices.exists(colIdx => {
        field.forall(row => row(colIdx) < 0)
      })
      isRowBingo || isColBingo
    }

    def markNumber(num: Int): Unit = {
      for {
        i <- field.indices
        j <- field(i).indices
      } yield {
        if (field(i)(j) == num) {
          field(i)(j) = -1
        }
      }
    }

    def getUnmarkedSum: Int = {
      field.flatten.filterNot(_ < 0).sum
    }
  }

  @tailrec
  def getBingoResultWinStrategy(bingoNumbers: List[Int], boards: Seq[Board]): Int = bingoNumbers match {
    case Nil => -1
    case num :: xs =>
      boards.foreach(_.markNumber(num))
      val winnerBoard: Option[Board] = boards.find(_.isBingo)
      winnerBoard match {
        case None => getBingoResultWinStrategy(xs, boards)
        case Some(b) => b.getUnmarkedSum * num
      }
  }

  @tailrec
  def getBingoResultLoseStrategy(bingoNumbers: List[Int], boards: Seq[Board]): Int = bingoNumbers match {
    case Nil => -1
    case num :: xs =>
      boards.foreach(_.markNumber(num))
      if (boards.forall(_.isBingo)) {
        boards.head.getUnmarkedSum * num
      } else {
        getBingoResultLoseStrategy(xs, boards.filterNot(_.isBingo))
      }
  }

  def main(args: Array[String]): Unit = {
    val inputLines = Source.fromResource("aoc2021/input_4.txt").getLines().toList

    val currentBoardLines: mutable.Buffer[String] = mutable.Buffer[String]()
    val bingoNumbers: List[Int] = inputLines.head.split(",").map(_.toInt).toList
    val boards: mutable.Buffer[Board] = mutable.Buffer[Board]()

    inputLines.tail.tail.foreach(line => {
      if (line.trim.isEmpty) {
        val boardFields = currentBoardLines.toArray.map(_.split(" ")
          .filter(_.nonEmpty).map(_.toInt))
        boards.append(new Board(boardFields))
        currentBoardLines.clear()
      } else {
        currentBoardLines.append(line)
      }
    })

    println(getBingoResultWinStrategy(bingoNumbers, boards.toSeq)) //10374
    println(getBingoResultLoseStrategy(bingoNumbers, boards.toSeq)) //24742
  }
}
