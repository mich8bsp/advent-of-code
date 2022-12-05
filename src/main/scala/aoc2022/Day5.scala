package aoc2022

import scala.collection.mutable

object Day5 {

  def solveA(stacks: Array[mutable.Stack[Char]], instructions: List[Instruction]): String = {
    instructions.foreach { instruction =>
      (0 until instruction.num).foreach { _ =>
        val c = stacks(instruction.from - 1).pop()
        stacks(instruction.to - 1).push(c)
      }
    }
    stacks.map(_.head).mkString
  }

  def solveB(input: Unit): Int = {
    ???
  }

  def parseStacks(inputLines: List[String]): Array[mutable.Stack[Char]] = {
    val linesRev = inputLines.reverse
    val stackNumByLineIdx: Map[Int, Int] = linesRev.head.zipWithIndex.filterNot(_._1 == ' ').map {
      case (x, idx) => idx -> x.toString.toInt
    }.toMap
    val stacks: Array[mutable.Stack[Char]] = Array.fill(stackNumByLineIdx.size)(mutable.Stack.empty[Char])
    inputLines.tail.foreach(line => {
      stackNumByLineIdx.foreach {
        case (idx, stackNum) =>
          val containerVal = line(idx)
          if (containerVal != ' ') {
            stacks(stackNum - 1).push(containerVal)
          }
      }
    })
    stacks
  }

  def parseInstructions(inputLines: List[String]): List[Instruction] = {
    inputLines.map(line => {
      val Array(_, num, _, from, _ , to) = line.split(" ")
      Instruction(num.toInt, from.toInt, to.toInt)
    })
  }

  case class Instruction(num: Int, from: Int, to: Int)

  def main(args: Array[String]): Unit = {
    val startingStacks :: instructionLines :: Nil = readSections(5) { identity }
    val stacks: Array[mutable.Stack[Char]] = parseStacks(startingStacks)
    val instructions: List[Instruction] = parseInstructions(instructionLines)
    println(solveA(stacks, instructions))
//    solveB(input)
  }
}
