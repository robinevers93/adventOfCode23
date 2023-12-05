package day4

import day4.Scratchcards.ScratchCard
import org.scalatest.wordspec.AnyWordSpec

class ScratchcardsTest extends AnyWordSpec {

  val testInput: List[ScratchCard] =
    general.ReadInTestData
      .getPuzzleInput(identity)("src/test/scala/day4/testData.txt")
      .map(Scratchcards.readScrachCard)

  "part1" should {
    "work" in {
      assert(Scratchcards.solve1(testInput) == 13L)
    }
  }

  "part2" should {
    "work" in {
      assert(Scratchcards.solve2(testInput) == 30)
    }
  }

}
