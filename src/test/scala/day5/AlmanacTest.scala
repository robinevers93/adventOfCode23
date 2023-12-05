package day5

import day5.Almanac.{RangeAlmanac, SimpleAlmanac}
import org.scalatest.wordspec.AnyWordSpec

class AlmanacTest extends AnyWordSpec {

  val testInputPart1: SimpleAlmanac =
    Almanac.readSimpleAlmanac(
      general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day5/testData.txt")
    )

  val testInputPart2: RangeAlmanac =
    Almanac.readAlmanac(
      general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day5/testData.txt")
    )

  "reading in a map data" should {
    "work" in {
      val line = "50 98 2"
      assert(Almanac.readMap(List(line)).follow(97) == 97)
      assert(Almanac.readMap(List(line)).follow(98) == 50)
      assert(Almanac.readMap(List(line)).follow(99) == 51)
      assert(Almanac.readMap(List(line)).follow(100) == 100)
    }
  }

  "part1" should {
    "work" in {
      assert(Almanac.solve1(testInputPart1) == 35)
    }
  }

  "part2" should {
    "work" in {
      assert(Almanac.solve2(testInputPart2) == 46)
    }
  }

}
