package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day4 {

  class Board(val field: Array[Array[Int]]){
    def isBingo: Boolean = {
      lazy val isRowBingo: Boolean = field.exists(_.forall(_ < 0))
      lazy val isColBingo: Boolean = field.indices.exists(colIdx => {
        field.forall(row => row(colIdx) < 0)
      })
      isRowBingo || isColBingo
    }

    def markNumber(num: Int): Unit = {
      for{
        i <- field.indices
        j <- field(i).indices
      }yield {
        if(field(i)(j) == num){
          field(i)(j) = -1
        }
      }
    }

    def getUnmarkedSum: Int = {
      field.flatten.filterNot(_ < 0).sum
    }
  }

  def getBingoResultWinStrategy(bingoNumbers: Array[Int], boards: mutable.Buffer[Day4.Board]): Int = {
    bingoNumbers.foreach(num => {
      boards.foreach(_.markNumber(num))
      boards.find(_.isBingo).foreach(bingoBoard => {
        return bingoBoard.getUnmarkedSum * num
      })
    })
    -1
  }

  def getBingoResultLoseStrategy(bingoNumbers: Array[Int], boards: mutable.Buffer[Day4.Board]): Int = {
    var boardsLeft: Seq[Board] = boards.toSeq
    bingoNumbers.foreach(num => {
      boardsLeft.foreach(_.markNumber(num))
      if(boardsLeft.forall(_.isBingo)){
        return boardsLeft.head.getUnmarkedSum * num
      }else{
        boardsLeft = boardsLeft.filterNot(_.isBingo)
      }
    })
    -1
  }

  def main(args: Array[String]): Unit = {
    val inputLines = Source.fromResource("input2021_4.txt").getLines().toList

    val currentBoardLines: mutable.Buffer[String] = mutable.Buffer[String]()
    val bingoNumbers: Array[Int] = inputLines.head.split(",").map(_.toInt)
    val boards: mutable.Buffer[Board] = mutable.Buffer[Board]()

    inputLines.tail.tail.foreach(line => {
      if(line.trim.isEmpty){
        val boardFields = currentBoardLines.toArray.map(_.split(" ")
          .filter(_.nonEmpty).map(_.toInt))
        boards.append(new Board(boardFields))
        currentBoardLines.clear()
      }else{
        currentBoardLines.append(line)
      }
    })

    println(getBingoResultLoseStrategy(bingoNumbers, boards))
  }
}
