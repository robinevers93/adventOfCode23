package day8

import day8.Wasteland.Network
import org.scalatest.wordspec.AnyWordSpec

class WastelandTest extends AnyWordSpec {

  val testData1: Network =
    Wasteland.readInput(
      general.ReadInTestData
        .getPuzzleInput(identity)("src/test/scala/day8/testData.txt")
    )

  val testData2: Network =
    Wasteland.readInput(
      general.ReadInTestData
        .getPuzzleInput(identity)("src/test/scala/day8/testData2.txt")
    )

  "part1" should {
    "work" in {
      assert(Wasteland.solve1(testData1) == 2)
    }
  }

  "part2" should {
    "work" in {
      assert(Wasteland.solve2(testData2) == 6)
    }
  }

}
