package day2

import org.scalatest.wordspec.AnyWordSpec

class CubesTest extends AnyWordSpec {

  "Solving the puzzle" should {
    "work" in {
      val input = general.ReadInTestData.getPuzzleInput(Cubes.readGame)("src/test/scala/day2/sampleData.txt")
      val totalRed = 12
      val totalGreen = 13
      val totalBlue = 14
      val possibleGames = Cubes.possibleGames(input, totalRed, totalGreen, totalBlue)
      val answer = Cubes.sumIds(possibleGames)
      assert(answer == 8)
      val required = Cubes.minimumRequiredCubes(input)
      assert(required.sum == 2286)
    }
  }

}
