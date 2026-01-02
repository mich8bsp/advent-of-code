package aoc2020

import scala.collection.mutable

object Day8 {
  sealed trait Instruction
  case class Nop(n: Int) extends Instruction
  case class Acc(n: Int) extends Instruction
  case class Jmp(n: Int) extends Instruction

  def parseInstruction(line: String): Instruction = {
    val Array(op, n) = line.split(" ")
    val delta: Int = n.replaceFirst("\\+", "").toInt
    op match {
      case "nop" => Nop(delta)
      case "acc" => Acc(delta)
      case "jmp" => Jmp(delta)
    }
  }

  def runProgram(instructions: List[Instruction]): (Int, Boolean) = {
    var accumulator = 0
    var pc = 0
    val pcEncountered = mutable.Set.empty[Int]
    while (!pcEncountered.contains(pc) && pc < instructions.length) {
      pcEncountered.add(pc)
      instructions(pc) match {
        case Nop(_) =>
          pc += 1
        case Acc(n) =>
          accumulator += n
          pc += 1
        case Jmp(n) =>
          pc += n
      }
    }

    (accumulator, pc >= instructions.length)
  }

  def solveA(instructions: List[Instruction]): Int = {
    runProgram(instructions)._1
  }

  def solveB(instructions: List[Instruction]): Int = {
    instructions.zipWithIndex.flatMap {
      case (Jmp(n), idx) =>
        val (res, success) = runProgram(instructions.slice(0, idx) ++ List(Nop(n)) ++ instructions.slice(idx + 1, instructions.length))
        Some(res).filter(_ => success)
      case (Nop(n), idx) =>
        val (res, success) = runProgram(instructions.slice(0, idx) ++ List(Jmp(n)) ++ instructions.slice(idx + 1, instructions.length))
        Some(res).filter(_ => success)
      case _ =>
        None
    }.head
  }

  def main(args: Array[String]): Unit = {
    val instructions: List[Instruction] = readFileLines(8)(parseInstruction)
    println(solveA(instructions))
    println(solveB(instructions))
  }
}
