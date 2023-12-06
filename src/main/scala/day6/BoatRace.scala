package day6

import scala.collection.immutable.NumericRange

object BoatRace {

  case class Race(time: Long, distance: Long)

  def readAsMultipleRaces(input: List[String]): List[Race] = {
    val ints = input.map(_.dropWhile(!_.isDigit).split("\\s+").map(_.toLong).toList)
    ints.head.zip(ints(1)).map { case (time, distance) => Race(time, distance) }
  }

  def readAsSingleRace(input: List[String]): Race =
    readAsMultipleRaces(input.map(_.replaceAll("\\s+", ""))).head

  def waysToWin(in: Race): Long =
    NumericRange(1L, in.time, 1L).map(time => time * (in.time - time)).count(_ > in.distance)

  def solve1(input: List[Race]): Long =
    input.map(waysToWin).product

}
