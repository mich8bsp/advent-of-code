package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day16 {

  trait Transmission {
    val binaryLength: Int
    val version: Int
    val packetId: Int
  }

  object Operator extends Enumeration {
    val SUM, PRODUCT, MINIMUM, MAXIMUM, GT, LT, ET = Value
  }
  case class OperatorTransmission(
                                   override val version: Int,
                                   override val packetId: Int,
                                   override val binaryLength: Int,
                                   subPackets: List[Transmission],
                                   operator: Operator.Value
                                 ) extends Transmission

  case class LiteralTransmission(
                                  override val version: Int,
                                  override val packetId: Int,
                                  literalValue: Long,
                                  override val binaryLength: Int
                                ) extends Transmission

  def parseTransmission(transmission: Array[Char]): Transmission = {
    val transmissionBinary = transmission.map(c => Integer.parseInt(c.toString, 16))
      .map(Integer.toBinaryString)
      .map(_.takeRight(4))
      .map(x => if(x.length<4) "0" * (4 - x.length) + x else x)
      .mkString

    parseTransmission(transmissionBinary)
  }

  def parseTransmission(transmissionBinary: String): Transmission = {

    val packetVersion = Integer.parseInt(transmissionBinary.slice(0, 3), 2)
    val packetId = Integer.parseInt(transmissionBinary.slice(3, 6), 2)

    packetId match {
      case 4 => parseLiteralTransmission(packetVersion, packetId, transmissionBinary)
      case x =>
        val operator: Operator.Value = x match {
          case 0 => Operator.SUM
          case 1 => Operator.PRODUCT
          case 2 => Operator.MINIMUM
          case 3 => Operator.MAXIMUM
          case 5 => Operator.GT
          case 6 => Operator.LT
          case 7 => Operator.ET
        }
        parseOperatorTransmission(packetVersion, packetId, operator, transmissionBinary)
    }

  }


  def parseLiteralTransmission(packetVersion: Int, packetId: Int, transmissionBinary: String): LiteralTransmission = {
    var i = 6
    var payload: String = ""
    while (transmissionBinary(i) == '1') {
      payload = payload + transmissionBinary.slice(i + 1, i + 5)
      i += 5
    }
    payload = payload + transmissionBinary.slice(i + 1, i + 5)
    LiteralTransmission(
      version = packetVersion,
      packetId = packetId,
      literalValue = java.lang.Long.parseLong(payload, 2),
      binaryLength = i + 5
    )
  }

  def parseOperatorTransmission(packetVersion: Int, packetId: Int, operator: Operator.Value, transmissionBinary: String): OperatorTransmission = {
    val lengthTypeId: Char = transmissionBinary(6)
    lengthTypeId match {
      case '0' =>
        val totalLengthInBits = Integer.parseInt(transmissionBinary.slice(7, 22), 2)
        var lengthInBitsLeft: Int = totalLengthInBits
        var i: Int = 22
        val subPackets: mutable.Buffer[Transmission] = mutable.Buffer[Transmission]()
        while(lengthInBitsLeft > 0) {
          val subPacketBinary: String = transmissionBinary.slice(i, i+lengthInBitsLeft)
          if(subPacketBinary.forall(_ == '0')){
            lengthInBitsLeft = 0
          }else{
            val subPacket: Transmission = parseTransmission(transmissionBinary.slice(i, i + lengthInBitsLeft))
            subPackets.append(subPacket)
            lengthInBitsLeft -= subPacket.binaryLength
            i += subPacket.binaryLength
          }
        }
        OperatorTransmission(
          version = packetVersion,
          packetId = packetId,
          binaryLength = 22 + totalLengthInBits,
          operator = operator,
          subPackets = subPackets.toList
        )

      case '1' =>
        val numOfSubPackets = Integer.parseInt(transmissionBinary.slice(7, 18), 2)
        var i = 18
        val subpackets = (0 until numOfSubPackets).map(_ => {
          val subPacket = parseTransmission(transmissionBinary.slice(i, transmissionBinary.length))
          i += subPacket.binaryLength
          subPacket
        }).toList

        OperatorTransmission(
          version = packetVersion,
          packetId = packetId,
          binaryLength = 18 + subpackets.map(_.binaryLength).sum,
          operator = operator,
          subPackets = subpackets)
    }
  }

  def countTransmissionVersions(transmission: Transmission): Int = transmission match {
    case x: LiteralTransmission => x.version
    case x: OperatorTransmission => x.version + x.subPackets.map(countTransmissionVersions).sum
  }

  def evaluate(transmission: Transmission): Long = transmission match {
    case x: LiteralTransmission => x.literalValue
    case x: OperatorTransmission => x.operator match {
      case Operator.SUM | Operator.PRODUCT | Operator.MINIMUM | Operator.MAXIMUM =>
        val reducer: List[Long] => Long = x.operator match {
          case Operator.SUM => _.sum
          case Operator.PRODUCT => _.product
          case Operator.MINIMUM => _.min
          case Operator.MAXIMUM => _.max
        }
        reducer(x.subPackets.map(evaluate))
      case _ =>
        val comparison: (Long, Long) => Boolean = x.operator match {
          case Operator.GT => _ > _
          case Operator.LT => _ < _
          case Operator.ET => _ == _
        }
        if(comparison(evaluate(x.subPackets.head), evaluate(x.subPackets(1)))){
          1
        }else{
          0
        }
    }
  }

  def parseInput(filePath: String): List[Array[Char]] = {
    Source.fromResource(filePath).getLines().toList.map(_.toCharArray)
  }


  def main(args: Array[String]): Unit = {
    val transmissions = parseInput("aoc2021/input_16.txt")

    transmissions.foreach(transmission => {
      println(countTransmissionVersions(parseTransmission(transmission)))
      println(evaluate(parseTransmission(transmission)))
    })
  }
}
