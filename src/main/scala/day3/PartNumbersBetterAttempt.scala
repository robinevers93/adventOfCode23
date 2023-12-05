package day3

import scala.util.matching.Regex

object PartNumbersBetterAttempt {

  private case class Match(value: String, x: Int, y: Int) {
    val xRange: Set[Int] = (x until x + value.length).toSet
  }

  def getPartNumbers(lines: List[String]): List[Int] =
    solve(lines.map(_.map(c => if """\d|\.""".r.matches(c.toString) then c else '*')), _.sum)

  def getGearRatios(lines: List[String]): List[Int] =
    solve(lines, partNumbers => if partNumbers.size == 2 then partNumbers.product else 0)

  private def getMatches(lines: List[String], regex: Regex): List[Match] =
    lines.zipWithIndex.flatMap((line, i) => regex.findAllMatchIn(line).map(m => Match(m.matched, m.start, i)))

  private def solve(lines: List[String], f: List[Int] => Int): List[Int] = {
    val numberMatches: List[Match] = getMatches(lines, """\d+""".r)
    val starMatches: List[Match] = getMatches(lines, """\*""".r)
    starMatches.map { m =>
      val xRange = ((m.x - 1) to (m.x + 1)).toSet
      val yRange = ((m.y - 1) to (m.y + 1)).toSet
      val partNumbers = numberMatches
        .filter(numberMatch => xRange.intersect(numberMatch.xRange).nonEmpty && yRange.contains(numberMatch.y))
        .map(_.value.toInt)
      f(partNumbers)
    }
  }

}
