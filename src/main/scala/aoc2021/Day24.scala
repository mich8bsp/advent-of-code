package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day24 {

  class ALU {
    val variableNames: Set[String] = Set("w", "x", "y", "z")

    val cache: mutable.Map[(Int, Long), (String, Boolean)] = mutable.Map()

    def runInstructions(subsections: List[List[String]], ordering: Int): (String, Boolean) = {
      val inputOptions = if (ordering < 0) 1L to 9L else 9L to 1L by -1L

      def runInstructionsSubsection(subsectionIdx: Int, z: Long): (String, Boolean) = {
        if (subsectionIdx == subsections.length) {
          ("", z == 0L)
        } else {
          cache.getOrElseUpdate((subsectionIdx, z), {
              var found: Option[String] = None
              val instructions = subsections(subsectionIdx)
              inputOptions.foreach(w => {
                if (found.isEmpty) {
                  var newState = (w, 0L, 0L, z)
                  instructions.tail.foreach(instruction => {
                    newState = runInstruction(instruction, newState)
                  })
                  val resWithCurrent = runInstructionsSubsection(subsectionIdx + 1, getVariable(newState, "z"))
                  if (resWithCurrent._2) {
                    found = Some(w.toString + resWithCurrent._1)
                  }
                }
              })

              found.map((_, true)).getOrElse(("", false))
            })
        }
      }

      runInstructionsSubsection(0, 0L)
    }

    def runInstruction(instruction: String, state: (Long, Long, Long, Long)): (Long, Long, Long, Long) = {
      val instructionParts = instruction.split(" ")
      instructionParts.head match {
        case "add" =>
          runBinaryInstruction(state, instructionParts(1), instructionParts(2), _ + _)
        case "mul" =>
          runBinaryInstruction(state, instructionParts(1), instructionParts(2), _ * _)
        case "div" =>
          runBinaryInstruction(state, instructionParts(1), instructionParts(2), _ / _)
        case "mod" =>
          runBinaryInstruction(state, instructionParts(1), instructionParts(2), _ % _)
        case "eql" =>
          runBinaryInstruction(state, instructionParts(1), instructionParts(2), (a, b) => if (a == b) 1 else 0)
        case "neq" =>
          runBinaryInstruction(state, instructionParts(1), instructionParts(2), (a, b) => if (a != b) 1 else 0)
        case "ass" =>
          runBinaryInstruction(state, instructionParts(1), instructionParts(2), (_, b) => b)
      }
    }

    def setVariable(state: (Long, Long, Long, Long), varName: String, value: Long): (Long, Long, Long, Long) = {
      varName match {
        case "w" => (value, state._2, state._3, state._4)
        case "x" => (state._1, value, state._3, state._4)
        case "y" => (state._1, state._2, value, state._4)
        case "z" => (state._1, state._2, state._3, value)
      }
    }

    def getVariable(state: (Long, Long, Long, Long), varName: String): Long = varName match {
      case "w" => state._1
      case "x" => state._2
      case "y" => state._3
      case "z" => state._4
    }

    def runBinaryInstruction(state: (Long, Long, Long, Long), firstOperandName: String, secondOperandName: String, operator: (Long, Long) => Long): (Long, Long, Long, Long) = {
      val firstOperandValue: Long = getVariable(state, firstOperandName)
      val secondOperandValue = if (variableNames.contains(secondOperandName)) {
        getVariable(state, secondOperandName)
      } else {
        secondOperandName.toLong
      }

      setVariable(state, firstOperandName, operator(firstOperandValue, secondOperandValue))
    }
  }

  class Monad(val alu: ALU, instructions: List[String]) {
    val optimizedInstructions = optimize(instructions)

    def optimize(unoptimizedInstructions: List[String]): List[String] = {
      val optimized = mutable.Buffer[String]()
      var i = 0
      while (i < unoptimizedInstructions.length) {
        val splitInstruction = unoptimizedInstructions(i).split(" ")
        if (splitInstruction.head != "div" || splitInstruction(2) != "1") {
          if (i < unoptimizedInstructions.length - 1) {
            val nextSplit = unoptimizedInstructions(i + 1).split(" ")
            if (splitInstruction.head == "eql" && nextSplit.head == "eql" &&
              splitInstruction(1) == nextSplit(1) && nextSplit(2) == "0") {
              optimized.addOne(s"neq ${splitInstruction(1)} ${splitInstruction(2)}")
              i += 1
            } else if (splitInstruction.head == "mul" && splitInstruction(2) == "0" &&
              nextSplit.head == "add" && nextSplit(1) == splitInstruction(1)) {
              optimized.addOne(s"ass ${nextSplit(1)} ${nextSplit(2)}")
              i += 1
            } else {
              optimized.addOne(unoptimizedInstructions(i))
            }
          } else {
            optimized.addOne(unoptimizedInstructions(i))
          }
        }

        i += 1
      }

      optimized.toList
    }

    def splitProgram(instructions: List[String]): List[List[String]] = {
      val indexOfNext = instructions.indexWhere(_.startsWith("inp"), 1)
      if (indexOfNext < 0) {
        List(instructions)
      } else {
        val (currSection, left) = instructions.splitAt(indexOfNext)
        currSection :: splitProgram(left)
      }
    }

    def findMaxValidNumber: Long = {
      val res = alu.runInstructions(splitProgram(instructions), ordering = 1)
      res._1.toLong
    }

    def findMinValidNumber: Long = {
      val res = alu.runInstructions(splitProgram(instructions), ordering = -1)
      res._1.toLong
    }
  }


  def main(args: Array[String]): Unit = {
    val monadInstructions: List[String] = Source.fromResource("aoc2021/input_24.txt").getLines().toList

    val monad = new Monad(new ALU, monadInstructions)

    println(monad.findMaxValidNumber)
    println(monad.findMinValidNumber)
  }
}
