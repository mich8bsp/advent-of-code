package aoc2020

import scala.util.Try

object Day4 {
  val mandatoryPassportFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  case class Passport(fields: Map[String, String])

  def parsePassports: List[Passport] = {
    readSections[Passport](4) { lines =>
      Passport(lines.flatMap(_.split(" "))
        .filter(_.nonEmpty)
        .map(line => (line.split(":").head, line.split(":")(1)))
        .toMap)
    }
  }

  def solveA(passports: List[Passport]): Int = {
    val validator: Passport => Boolean = (p: Passport) => mandatoryPassportFields.subsetOf(p.fields.keySet)

    passports.count(validator)
  }

  def solveB(passports: List[Passport]): Int = {
    val validator: Passport => Boolean = (p: Passport) => {
      lazy val allFieldsPresent = mandatoryPassportFields.subsetOf(p.fields.keySet)

      lazy val fieldsValid = p.fields.map {
        case ("byr", v) => Try(v.toInt).filter((1920 to 2002).contains).isSuccess
        case ("iyr", v) => Try(v.toInt).filter((2010 to 2020).contains).isSuccess
        case ("eyr", v) => Try(v.toInt).filter((2020 to 2030).contains).isSuccess
        case ("hgt", v) =>
          (v.endsWith("cm") && Try(v.dropRight(2).toInt).filter((150 to 193).contains).isSuccess) ||
          (v.endsWith("in") && Try(v.dropRight(2).toInt).filter((59 to 76).contains).isSuccess)
        case ("hcl", v) => v.length == 7 && v.startsWith("#") && v.tail.matches("[0-9a-f]+")
        case ("ecl", v) => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(v)
        case ("pid", v) => v.length == 9 && Try(v.toLong).isSuccess
        case _ => true
      }.reduce(_ && _)

      allFieldsPresent && fieldsValid
    }

    passports.count(validator)
  }

  def main(args: Array[String]): Unit = {
    val passports = parsePassports
    println(solveA(passports))
    println(solveB(passports))
  }
}
