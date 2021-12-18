package aoc2021

import scala.io.Source
import scala.util.{Success, Try}

object Day18 {

  sealed trait SnailNumber {
    def +(other: SnailNumber): PairSnailNumber = {
      val sum = new PairSnailNumber(this, other, 0)
      this.incrementDepth(1)
      other.incrementDepth(1)
      this.parent = Some(sum)
      other.parent = Some(sum)
      sum
    }

    def explodeIfExplodable(): Boolean

    def splitIfSplittable(): Boolean

    def performReduce(): Boolean = explodeIfExplodable() || splitIfSplittable()

    var parent: Option[PairSnailNumber] = None
    var depth: Int

    def isExplodable: Boolean

    def isSplittable: Boolean

    def addToLeftMost(x: Int, origin: Option[SnailNumber] = None): Unit

    def addToRightMost(x: Int, origin: Option[SnailNumber] = None): Unit

    def magnitude: Int

    def incrementDepth(delta: Int): Unit
  }

  class RegularSnailNumber(_value: Int, _depth: Int) extends SnailNumber {
    var value: Int = _value
    var depth: Int = _depth

    def split: PairSnailNumber = {
      val splitLeftPart = new RegularSnailNumber(value / 2, depth + 1)
      val splitRightPart = new RegularSnailNumber(math.ceil(value / 2D).toInt, depth + 1)
      val splitPair = new PairSnailNumber(splitLeftPart, splitRightPart, depth)
      splitLeftPart.parent = Some(splitPair)
      splitRightPart.parent = Some(splitPair)
      splitPair.parent = this.parent
      splitPair
    }

    override def explodeIfExplodable(): Boolean = false

    override def splitIfSplittable(): Boolean = false

    override def isExplodable: Boolean = false

    override def isSplittable: Boolean = value >= 10

    override def addToLeftMost(x: Int, origin: Option[SnailNumber]): Unit = this.value += x

    override def addToRightMost(x: Int, origin: Option[SnailNumber]): Unit = this.value += x

    override def magnitude: Int = value

    override def toString: String = value.toString

    override def incrementDepth(delta: Int): Unit = depth += delta
  }

  class PairSnailNumber(_left: SnailNumber, _right: SnailNumber, _depth: Int) extends SnailNumber {
    var left: SnailNumber = _left
    var right: SnailNumber = _right
    var depth: Int = _depth

    def addToLeftMost(x: Int, origin: Option[SnailNumber] = None): Unit = {
      origin match {
        case Some(o) if o == right => parent.foreach(p => p.addToLeftMost(x, Some(this)))
        case Some(o) if o == left => right.addToLeftMost(x)
        case _ => left.addToLeftMost(x)
      }
    }

    def addToRightMost(x: Int, origin: Option[SnailNumber] = None): Unit = {
      origin match {
        case Some(o) if o == left => parent.foreach(p => p.addToRightMost(x, Some(this)))
        case Some(o) if o == right => left.addToRightMost(x)
        case _ => right.addToRightMost(x)
      }
    }

    def explodeIfExplodable(): Boolean = {
      lazy val leftExplosionRes: Boolean = if (left.isExplodable) {
        val valueToPropagateLeft: Int = left.asInstanceOf[PairSnailNumber].left.asInstanceOf[RegularSnailNumber].value
        val valueToPropagateRight: Int = left.asInstanceOf[PairSnailNumber].right.asInstanceOf[RegularSnailNumber].value
        this.left = new RegularSnailNumber(0, depth + 1)
        this.left.parent = Some(this)
        if (valueToPropagateRight > 0) {
          right.addToLeftMost(valueToPropagateRight)
        }
        if (valueToPropagateLeft > 0) {
          parent.foreach(_.addToRightMost(valueToPropagateLeft, origin = Some(this)))
        }
        true
      } else {
        left.explodeIfExplodable()
      }

      lazy val rightExplosionRes: Boolean = if (right.isExplodable) {
        val valueToPropagateLeft: Int = right.asInstanceOf[PairSnailNumber].left.asInstanceOf[RegularSnailNumber].value
        val valueToPropagateRight: Int = right.asInstanceOf[PairSnailNumber].right.asInstanceOf[RegularSnailNumber].value
        this.right = new RegularSnailNumber(0, depth + 1)
        this.right.parent = Some(this)
        if (valueToPropagateLeft > 0) {
          left.addToRightMost(valueToPropagateLeft)
        }
        if (valueToPropagateRight > 0) {
          parent.foreach(_.addToLeftMost(valueToPropagateRight, origin = Some(this)))
        }
        true
      } else {
        right.explodeIfExplodable()
      }

      leftExplosionRes || rightExplosionRes
    }

    def splitIfSplittable(): Boolean = {
      val leftSplitResult: Boolean = if (left.isSplittable) {
        this.left = left.asInstanceOf[RegularSnailNumber].split
        true
      } else {
        left.splitIfSplittable()
      }


      val rightSplitResult: Boolean = if(leftSplitResult){
        false
      } else {
        if (right.isSplittable) {
          this.right = right.asInstanceOf[RegularSnailNumber].split
          true
        } else {
          right.splitIfSplittable()
        }
      }

      leftSplitResult || rightSplitResult
    }

    override def isExplodable: Boolean = {
      depth == 4 && ((left, right) match {
        case (_: RegularSnailNumber, _: RegularSnailNumber) => true
        case _ => false
      })
    }

    override def isSplittable: Boolean = false

    override def magnitude: Int = left.magnitude * 3 + right.magnitude * 2

    override def incrementDepth(delta: Int): Unit = {
      depth += delta
      left.incrementDepth(delta)
      right.incrementDepth(delta)
    }

    override def toString: String = s"[${left.toString}, ${right.toString}]"
  }

  def sumNumbers(numbers: List[SnailNumber]): SnailNumber = {
    numbers.map(curr => {
      while (curr.performReduce()) { }
      curr
    }).reduceLeft((prev, curr) => {
      val sum = prev + curr
      while (sum.performReduce()) { }
      sum
    })
  }

  def getLargestPairSumMagnitude(numbers: () => List[SnailNumber]): Int = {
    val size = numbers().length

    (for {
      i <- 0 until size
      j <- 0 until size if i!=j
    }yield {
      val parsedNumbers: List[SnailNumber] = numbers()
      sumNumbers(List(parsedNumbers(i), parsedNumbers(j))).magnitude
    }).max
  }


  def isBalanced(str: String): Boolean = {
    str.count(_ == '[') == str.count(_ == ']')
  }

  def parseSnailNumber(line: String, depth: Int = 0): SnailNumber = {
    Try(line.toInt) match {
      case Success(n) => new RegularSnailNumber(n, depth)
      case _ => {
        (0 until line.length).foreach(i => {
          if (line(i) == ',') {
            val leftStr: String = line.slice(1, i).trim
            val rightStr: String = line.slice(i + 1, line.length - 1).trim
            if (isBalanced(leftStr) && isBalanced(rightStr)) {
              val left = parseSnailNumber(leftStr, depth + 1)
              val right = parseSnailNumber(rightStr, depth + 1)
              val parent = new PairSnailNumber(left, right, depth)
              left.parent = Some(parent)
              right.parent = Some(parent)
              return parent
            }
          }
        })
        throw new Exception("wtf")
      }
    }
  }


  def parseInput(filePath: String): List[SnailNumber] = {
    Source.fromResource(filePath).getLines().toList.map(parseSnailNumber(_))
  }

  def calculateSumMagnitude(): Unit = {
    val numbersTest = parseInput("input_2021_18_test.txt")
    println(sumNumbers(numbersTest).magnitude)

    val numbers = parseInput("input_2021_18.txt")
    println(sumNumbers(numbers).magnitude)
  }

  def calculateLargestPairSumMagnitude(): Unit = {
    val getNumbersTest = () => parseInput("input_2021_18_test.txt")
    println(getLargestPairSumMagnitude(getNumbersTest))

    val getNumbers = () => parseInput("input_2021_18.txt")
    println(getLargestPairSumMagnitude(getNumbers))
  }

  def main(args: Array[String]): Unit = {
    calculateSumMagnitude()
    calculateLargestPairSumMagnitude()
  }
}
