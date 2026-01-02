package aoc2021

import scala.collection.mutable
import scala.io.Source

object Day6 {

  type Days = Long
  type Timer = Int
  private val cache: mutable.Map[(Timer, Days), Long] = mutable.Map[(Timer, Days), Long]()

  def simulateLanternfish(timers: List[Timer], daysLeft: Days): Long = {

    def simulateSingle(timer: Timer, daysLeft: Days): Long = {
      cache.getOrElseUpdate((timer, daysLeft), {
        val updatedTimers: List[Timer] = timer match {
          case 0 => List(6, 8)
          case t => List(t-1)
        }
        if(daysLeft-1 == 0){
          updatedTimers.size
        }else{
          updatedTimers.map(simulateSingle(_, daysLeft-1)).sum
        }
      })
    }

    timers.map(simulateSingle(_, daysLeft)).sum
  }


  def main(args: Array[String]): Unit = {
    val initialTimers: List[Int] = readFileLines[List[Int]](6).flatten

    println(simulateLanternfish(initialTimers, 256))
  }
}
