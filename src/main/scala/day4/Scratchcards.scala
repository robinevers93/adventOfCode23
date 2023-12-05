package day4

import scala.annotation.tailrec

object Scratchcards {

  case class ScratchCard(id: Int, winningNumbers: List[Int], cardNumbers: List[Int])

  def readScrachCard(input: String): ScratchCard = {
    val pattern = """Card (.+): (.*) \| (.*)""".r
    input match {
      case pattern(id, winningNumbers, cardNumbers) =>
        ScratchCard(
          id.trim.toInt,
          winningNumbers.split(" ").flatMap(_.trim.toIntOption).toList,
          cardNumbers.split(" ").flatMap(_.trim.toIntOption).toList
        )
    }
  }

  private def numberOfWinningCards(input: ScratchCard): Int =
    input.cardNumbers.count(input.winningNumbers.contains(_))

  def solve1(input: List[ScratchCard]): Int =
    input.map(card => math.pow(2.toDouble, numberOfWinningCards(card).toDouble - 1).toInt).sum

  def solve2(input: List[ScratchCard]): Int = {
    @tailrec
    def go(acc: List[ScratchCard], total: Int = 0): Int = {
      acc.headOption match {
        case Some(card) =>
          val cardOccurrences = acc.count(_.id == card.id)
          val newCards = List.fill(cardOccurrences)((card.id until card.id + numberOfWinningCards(card)).map(input(_)).toList).flatten
          val newAcc = acc.filter(c => c.id != card.id) ++ newCards
          go(newAcc, total + cardOccurrences)
        case None => total
      }
    }
    go(input)
  }

}
