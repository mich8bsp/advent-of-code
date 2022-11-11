package aoc2021

object Day21 {

  class DeterministicDie {
    private var value: Int = 0
    var rollsCounter: Int = 0

    def roll(): Int = {
      val rolledValue = if(value == 100) 1 else value+1
      rollsCounter += 1
      value = rolledValue
      value
    }
  }

  object QuantumDie {
    private val rollsSumToNumOfUniverses: Map[Int, Long] = {
      val sumCombinations = for{
        first <- Seq(1,2,3)
        second <- Seq(1,2,3)
        third <- Seq(1,2,3)
      }yield {
        first + second + third
      }

      sumCombinations.groupBy(identity).view.mapValues(_.size.toLong).toMap
    }

    def getSumOfThreeRollsWithNumOfUniverses: Seq[(Int, Long)] = {
      rollsSumToNumOfUniverses.toSeq
    }
  }

  class Player(val id: Int){
    def copy(): Player = {
      val p = new Player(id)
      p.score = score
      p.position = position
      p
    }

    var position: Int = 0
    var score: Int = 0

    def move(steps: Int): Unit = {
      position = ((position + steps - 1) % 10) + 1
      score += position
    }
  }

  def parseInput(isTest: Boolean = false): List[Player] = {
    readFileLines[Player](21, isTest = isTest) { line =>
      val splitLine = line.split(" ")
      val player = new Player(splitLine(1).toInt)
      player.position = splitLine(4).toInt
      player
    }
  }

  def playGameDeterministic(players: List[Player], die: DeterministicDie): Int = {
    var playerIdx: Int = 0
    while(true){
      val currPlayer = players(playerIdx)
      val stepsToMove: Int = die.roll() + die.roll() + die.roll()
      currPlayer.move(stepsToMove)
      if(currPlayer.score >= 1000){
        return players.find(_.score < 1000).get.score * die.rollsCounter
      }
      playerIdx = (playerIdx + 1) % players.length
    }

    Int.MinValue
  }



  def playGameQuantum(player1: Player, player2: Player, playerIdx: Int = 0): (Long, Long) = {
    QuantumDie.getSumOfThreeRollsWithNumOfUniverses.map({
      case (sum, numOfUniverses) =>
        val currPlayerCopy = (if(playerIdx==0) player1 else player2).copy()
        currPlayerCopy.move(sum)
        val numOfWinsWithCurrentMove: (Long, Long) = if(currPlayerCopy.score >= 21){
          if(playerIdx == 0){
            (1L, 0L)
          }else{
            (0L, 1L)
          }
        }else{
         if(playerIdx == 0) {
            playGameQuantum(currPlayerCopy, player2, (playerIdx + 1) % 2)
          } else {
            playGameQuantum(player1, currPlayerCopy, (playerIdx + 1) % 2)
          }
        }
        (numOfWinsWithCurrentMove._1 * numOfUniverses, numOfWinsWithCurrentMove._2 * numOfUniverses)
    }).reduce((x,y) => (x._1 + y._1, x._2 + y._2))
  }

  def main(args: Array[String]): Unit = {
    var players = parseInput()
    val die = new DeterministicDie
    println(playGameDeterministic(players, die))

    players = parseInput()
    println(playGameQuantum(players.head, players(1)))
  }
}
