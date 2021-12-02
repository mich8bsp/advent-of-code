package aoc2021

import scala.io.Source

object Direction extends Enumeration {
  val UP, DOWN, FORWARD = Value
}

case class Command(direction: Direction.Value, steps: Int)

object Day2 {

  def executeCommands(commands: List[Command]): (Int, Int) = {
    commands.foldLeft((0, 0))((prev, command) => command.direction match {
      case Direction.UP => (prev._1, prev._2 - command.steps)
      case Direction.DOWN => (prev._1, prev._2 + command.steps)
      case Direction.FORWARD => (prev._1 + command.steps, prev._2)
    })
  }

  def executeCommandsFixed(commands: List[Command]): (Int, Int) = {
    val res = commands.foldLeft((0, 0, 0))((prev, command) => command.direction match {
      case Direction.UP => (prev._1, prev._2, prev._3 - command.steps)
      case Direction.DOWN => (prev._1, prev._2, prev._3 + command.steps)
      case Direction.FORWARD => (prev._1 + command.steps, prev._2 + (prev._3 * command.steps) ,prev._3)
  })
    (res._1, res._2)
  }

  def main(args: Array[String]): Unit = {
    val commands = Source.fromResource("input2021_2.txt").getLines().toList
      .map(line => {
        val Array(dirStr, stepsStr) = line.split(" ")
        Command(
          direction = Direction.withName(dirStr.toUpperCase),
          steps = stepsStr.toInt
        )
      })

    val res = executeCommands(commands)
    println(res)
    println(res._1 * res._2) //2073315

    val res2 = executeCommandsFixed(commands)
    println(res2)
    println(res2._1 * res2._2) //1840311528
  }
}
