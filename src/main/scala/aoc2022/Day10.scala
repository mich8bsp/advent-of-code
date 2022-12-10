package aoc2022

import scala.annotation.tailrec

object Day10 {

  sealed trait Instruction {
    val cycles: Int
  }
  case object Noop extends Instruction {
    override val cycles: Int = 1
  }
  case class AddX(n: Int, cycles: Int = 2) extends Instruction

  def runCycle(instructions: List[Instruction], register: Int): (List[Instruction], Int) = instructions match {
      case Nil => (Nil, register)
      case Noop :: xs => (xs, register)
      case AddX(n, cyclesLeft) :: xs if cyclesLeft > 1 =>
        (AddX(n, cyclesLeft - 1) :: xs, register)
      case AddX(n, 1) :: xs =>
        (xs, register + n)
    }

  @tailrec
  def solveA(instructions: List[Instruction], cycle: Int = 1, signalStrength: Int = 0, register: Int = 1): Int = {
    val (instructionsLeft: List[Instruction], updatedRegister: Int) = runCycle(instructions, register)

    val updatedSignalStrength: Int = signalStrength + (if ((cycle - 20) % 40 == 0) cycle * register else 0)
    instructionsLeft match {
      case Nil => updatedSignalStrength
      case _ => solveA(instructionsLeft, cycle + 1, updatedSignalStrength, updatedRegister)
    }
  }

  def solveB(instructions: List[Instruction]): String = {
    val rows = 6
    val cols = 40
    val crt: Array[Array[Char]] = Array.fill(rows, cols) { '.' }

    @tailrec
    def render(instructionsLeft: List[Instruction], cycle: Int = 1, register: Int = 1, renderIdx: Int = 0): Unit = {
      val spritePosition: Seq[Int] = (register  - 1 to register + 1).map(_ % 40)
      val pixel: Char = if (spritePosition.contains(renderIdx % 40)) '#' else '.'
      val (nextCycleInstructions: List[Instruction], updatedRegister: Int) = if (instructionsLeft.nonEmpty) {
        runCycle(instructionsLeft, register)
      } else {
        (Nil, register)
      }
      crt(renderIdx / cols)(renderIdx % cols) = pixel

      if (renderIdx < rows * cols - 1) {
        render(nextCycleInstructions, cycle + 1, updatedRegister, renderIdx + 1)
      }
    }

    render(instructions)
    crt.map(_.mkString(""))
      .mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val instructions: List[Instruction] = readFileLines[Instruction](10) {
      case "noop" => Noop
      case line if line.startsWith("addx") => AddX(line.split(" ").last.toInt)
    }

    println(solveA(instructions))
    println(solveB(instructions))
  }
}
