package day6

import day6.BoatRace.Race
import org.scalatest.wordspec.AnyWordSpec

class BoatRaceTest extends AnyWordSpec {

  val inputAsRaces: List[Race] =
    BoatRace.readAsMultipleRaces(
      general.ReadInTestData
        .getPuzzleInput(identity)("src/test/scala/day6/testData.txt")
    )

  val inputAsRace: Race =
    BoatRace.readAsSingleRace(
      general.ReadInTestData
        .getPuzzleInput(identity)("src/test/scala/day6/testData.txt")
    )

  "part1" should {
    "be the product of the ways wen reading the integers as individual races" in {
      assert(BoatRace.solve1(inputAsRaces) == 288)
    }
  }

  "part2" should {
    "be the number of ways to win when concatenating the integers" in {
      assert(BoatRace.waysToWin(inputAsRace) == 71503)
    }
  }

}
