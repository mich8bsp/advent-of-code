package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day3 {

  case class DiagnosticsCount(numOfZero: Int, numOfOne: Int){
    def getMostCommonBit: Char = if(numOfOne >= numOfZero) '1' else '0'
    def getLeastCommonBit: Char = if(numOfZero <= numOfOne) '0' else '1'
  }

  def countBitsInDiagnosticsAtIndex(diagnostics: List[String], bitIndex: Int): DiagnosticsCount = {
    val bitsAtIndex: String = diagnostics.map(_(bitIndex)).mkString
    DiagnosticsCount(
      numOfZero = bitsAtIndex.count(_ == '0'),
      numOfOne = bitsAtIndex.count(_ == '1')
    )
  }

  def countBitsInDiagnostics(diagnostics: List[String]): List[DiagnosticsCount] = {
    (0 until diagnostics.head.length).map(bitIndex => {
      countBitsInDiagnosticsAtIndex(diagnostics, bitIndex)
    }).toList
  }

  def getGammaEpsilonRate(diagnostics: List[String], bitPicker: DiagnosticsCount => Char): Int = {
    val bitsStr: String = countBitsInDiagnostics(diagnostics)
      .map(bitPicker)
      .mkString
    Integer.parseInt(bitsStr, 2)
  }

  def getGammaRate(diagnostics: List[String]): Int = {
    getGammaEpsilonRate(diagnostics, _.getMostCommonBit)
  }

  def getEpsilonRate(diagnostics: List[String]): Int = {
    getGammaEpsilonRate(diagnostics, _.getLeastCommonBit)
  }

  def getLifeSupportRating(diagnostics: List[String], bitPicker: DiagnosticsCount => Char): Int = {

    @tailrec
    def filterDiagnosticsByBitAtIndex(diagnosticsLeft: List[String], idx: Int = 0): String = {
      if(diagnosticsLeft.size == 1){
        diagnosticsLeft.head
      }else{
        val bitCount: DiagnosticsCount = countBitsInDiagnosticsAtIndex(diagnosticsLeft, idx)
        val bitToFilterBy: Char = bitPicker(bitCount)
        val filtered = diagnosticsLeft.filter(str => str(idx) == bitToFilterBy)
        filterDiagnosticsByBitAtIndex(filtered, idx + 1)
      }
    }

    Integer.parseInt(filterDiagnosticsByBitAtIndex(diagnostics), 2)
  }

  def getOxygenGeneratorRating(diagnostics: List[String]): Int = {
    getLifeSupportRating(diagnostics, _.getMostCommonBit)
  }

  def getCo2ScrubberRating(diagnostics: List[String]): Int = {
    getLifeSupportRating(diagnostics, _.getLeastCommonBit)
  }

  def main(args: Array[String]): Unit = {
    val diagnostics = readFileLines[String](3)
    println(getGammaRate(diagnostics) * getEpsilonRate(diagnostics))

    println(getOxygenGeneratorRating(diagnostics) * getCo2ScrubberRating(diagnostics))
  }
}
