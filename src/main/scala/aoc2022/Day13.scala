package aoc2022

import scala.collection.mutable
import scala.util.Try

object Day13 {

  sealed trait Packet
  case class IntPacket(x: Int) extends Packet
  case class ListPacket(inner: List[Packet]) extends Packet

  implicit val packetOrdering: Ordering[Packet] = new Ordering[Packet] {
    override def compare(left: Packet, right: Packet): Int = {
      (left, right) match {
        case (IntPacket(l), IntPacket(r)) => l.compareTo(r)
        case (l: IntPacket, r: ListPacket) => compare(ListPacket(List(l)), r)
        case (l: ListPacket, r: IntPacket) => compare(l, ListPacket(List(r)))
        case (l: ListPacket, r: ListPacket) =>
          l.inner.zip(r.inner).map((compare _).tupled)
            .foldLeft(0) {
              case (0, next) => next
              case (res, _) => res
            } match {
            case 0 => l.inner.size.compareTo(r.inner.size)
            case res => res
          }
      }
    }
  }



  def parsePacket(str: String): Packet = {
    str.trim match {
      case "[]" => ListPacket(Nil)
      case x if Try(x.toInt).isSuccess => IntPacket(x.toInt)
      case _ =>
        var balance = 0
        var elementStartIdx = 0
        val elementsBuffer = mutable.Buffer.empty[Packet]
        (0 until str.length).foreach { idx =>
            str(idx) match {
              case '[' =>
                if (balance == 0) {
                  elementStartIdx = idx + 1
                }
                balance += 1
              case ']' =>
                if (idx == str.length - 1) {
                  elementsBuffer.append(parsePacket(str.slice(elementStartIdx, idx)))
                }
                balance -= 1
              case _ if idx == str.length - 1 =>
                elementsBuffer.append(parsePacket(str.slice(elementStartIdx, idx + 1)))
              case ',' if balance == 0 =>
                elementsBuffer.append(parsePacket(str.slice(elementStartIdx, idx)))
                elementStartIdx = idx + 1
              case _ =>
            }
        }
        ListPacket(elementsBuffer.toList)
    }
  }

  def solveA(packets: List[(Packet, Packet)]): Int = {
    packets.zipWithIndex.collect {
      case ((leftPacket, rightPacket), idx) if packetOrdering.lt(leftPacket, rightPacket) =>
        idx + 1
    }.sum
  }

  def solveB(packets: List[Packet], dividerPackets: List[Packet]): Int = {
    val packetsSorted: List[Packet] = packets.sorted
    dividerPackets.map(packetsSorted.indexOf)
      .map(_ + 1)
      .product
  }

  def main(args: Array[String]): Unit = {
    val packetPairs: List[(Packet, Packet)] = readSections(13) { sectionLines =>
      val left = parsePacket(sectionLines.head)
      val right = parsePacket(sectionLines(1))
      (left, right)
    }
    println(solveA(packetPairs))
    val dividerPackets = List("[[2]]", "[[6]]").map(parsePacket)
    val allPackets = packetPairs.flatMap(x => List(x._1, x._2)) ++ dividerPackets
    println(solveB(allPackets, dividerPackets))
  }
}
