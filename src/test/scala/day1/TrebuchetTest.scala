package day1

import org.scalatest.wordspec.AnyWordSpec

class TrebuchetTest extends AnyWordSpec {

  "getCalibrationValue" should {
    "create a two-digit number out of the first and last number on each line" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day1/sampleData1.txt")
      val getCalibrationValues = input.map(Trebuchet.getCalibrationValue(_, Trebuchet.part1Regex))
      val answer = getCalibrationValues.sum
      assert(answer == 142)
    }
    "also take written out number into account" in {
      val input = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day1/sampleData2.txt")
      val getCalibrationValues = input.map(Trebuchet.getCalibrationValue(_, Trebuchet.part2Regex))
      val answer = getCalibrationValues.sum
      assert(answer == 281)
    }
  }

}
