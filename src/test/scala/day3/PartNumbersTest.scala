package day3

import org.scalatest.wordspec.AnyWordSpec

class PartNumbersTest extends AnyWordSpec {

  val testInput: List[List[Char]] = PartNumbersFirstAttempt.normaliseSpecialCharacters(
    general.ReadInTestData
      .getPuzzleInput(identity)("src/test/scala/day3/sampleData.txt")
      .map(_.toCharArray.toList)
  )

  val testInputAsString: List[String] = general.ReadInTestData.getPuzzleInput(identity)("src/test/scala/day3/sampleData.txt")

  val emptyLine = List.fill(10)('.')
  val line1 = testInput.head
  val line2 = testInput(1)
  val line3 = testInput(2)
  val line4 = testInput(3)
  val line8 = testInput(7)
  val line9 = testInput(8)
  val line10 = testInput(9)

  "numberStringToValue" should {
    "work" in {
      assert(PartNumbersFirstAttempt.numberStringToValue("123") == 123)
      assert(PartNumbersFirstAttempt.numberStringToValue("123#5") == 128)
    }
  }

  "sumPartNumbersForLine" should {
    "work" in {
      assert(PartNumbersFirstAttempt.sumPartNumbersForLine(line1, line2, emptyLine) == 467)
      assert(PartNumbersFirstAttempt.sumPartNumbersForLine(line2, line3, line1) == 0)
      assert(PartNumbersFirstAttempt.sumPartNumbersForLine(line3, line4, line2) == 668)
      assert(PartNumbersFirstAttempt.sumPartNumbersForLine(line10, emptyLine, line9) == 1262)
    }
  }

  "sumPartNumbers" should {
    "work" in {
      assert(PartNumbersFirstAttempt.sumPartNumbers(testInput) == 4361)
    }
  }

  "getGearRatios" should {
      "work" in {
      assert(PartNumbersBetterAttempt.getGearRatios(testInputAsString).sum == 467835)
      }
  }

  "getPartNumbers" should {
      "work" in {
      assert(PartNumbersBetterAttempt.getPartNumbers(testInputAsString).sum == 4361)
      }
  }

}
