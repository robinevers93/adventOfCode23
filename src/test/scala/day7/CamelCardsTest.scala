package day7

import day7.CamelCards.{CamelCard, CardCombination}
import org.scalatest.wordspec.AnyWordSpec

class CamelCardsTest extends AnyWordSpec {

  val input: List[CamelCard] =
    CamelCards.readInput(
      general.ReadInTestData
        .getPuzzleInput(identity)("src/test/scala/day7/testData.txt")
    )

  "part1" should {
    "work out what card combination is in a hand" in {
      assert(input.head.cardCombination == CardCombination.OnePair)
      assert(input(3).cardCombination == CardCombination.TwoPair)
      assert(input(3).bestJokerReplacement.cardCombination == CardCombination.FourOfAKind)
      assert(input(4).cardCombination == CardCombination.ThreeOfAKind)
      assert(input(4).bestJokerReplacement.cardCombination == CardCombination.FourOfAKind)
    }
    "work out what which hand is stronger" in {
      assert(input(3).bestJokerReplacement.card == List('K', 'T', 'T', 'T', 'T'))
      assert(input(4).bestJokerReplacement.card == List('Q', 'Q', 'Q', 'Q', 'A'))
      assert(input(3).isWinningCardWithJokerReplacement(input(4)))
    }
    "solve puzzle" in {
      assert(CamelCards.solve(input, false) == 6440)
    }
    "solve part 2" in {
      assert(CamelCards.solve(input, true) == 5905)
    }
  }

}