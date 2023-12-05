package day1

import scala.util.matching.Regex

object Trebuchet {

  private val numberStrings = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )

  val part1Regex: Regex = s"""([1-9])""".r
  val part2Regex: Regex = s"""(?=([1-9]|${numberStrings.keys.mkString("|")}))""".r

  def getCalibrationValue(line: String, regex: Regex): Int = {
    val numbers = regex.findAllMatchIn(line).map(_.group(1)).toList
    (numberStrings.getOrElse(numbers.head, numbers.head) + numberStrings.getOrElse(numbers.last, numbers.last)).toInt
  }

}
