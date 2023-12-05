package day3

import scala.annotation.tailrec

object PartNumbersFirstAttempt {

  private val emptyLine = List.fill(140)('.')

  def normaliseSpecialCharacters(lines: List[List[Char]]): List[List[Char]] =
    lines.map(_.map(c => if """\d|\.""".r.matches(c.toString) then c else '#'))

  def sumPartNumbers(lines: List[List[Char]]): Int = {
    @tailrec
    def go(lines: List[List[Char]], acc: Int = 0): Int =
      lines match {
        case previousLine :: currentLine :: nextLine :: _ => go(lines.tail, acc + sumPartNumbersForLine(currentLine, nextLine, previousLine))
        case _ => acc
      }
    go(lines.appended(emptyLine).prepended(emptyLine))
  }

  def sumPartNumbersForLine(line: List[Char], nextLine: List[Char], previousLine: List[Char]): Int = {
    val firstNumber = line.zipWithIndex

    @tailrec
    def go(list: List[(Char, Int)], acc: Int = 0): Int =
      val dropDots = list.dropWhile(_._1 == '.')
      val characters = dropDots.takeWhile(_._1 != '.')
      val range = characters.map(_._2)
      val number = characters.map(_._1).mkString
      if range.isEmpty then acc
      else
        val value =
          if number.contains('#')
          then numberStringToValue(number)
          else if nextLine.slice(range.head - 1, range.last + 2).exists(_ != '.') | previousLine.slice(range.head - 1, range.last + 2).exists(_ != '.')
          then numberStringToValue(number)
          else 0
        go(dropDots.drop(range.size), acc + value)

    go(firstNumber)
  }

  def numberStringToValue(string: String): Int = {
    val partNumbers = string.split('#').flatMap(_.toIntOption)
    println(s"partNumbers found: ${partNumbers.mkString(" ")}")
    partNumbers.sum
  }

}
