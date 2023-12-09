package day7

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.{Failure, Success, Try}

object CamelCards {

  def readInput(input: List[String]): List[CamelCard] =
    input.map(s =>
      s.split(" ") match {
        case Array(card, bid) => CamelCard(card.toList, bid.toInt)
        case _ => throw new Exception("Invalid input")
      }
    )

  enum CardCombination {
    case FiveOfAKind
    case FourOfAKind
    case FullHouse
    case ThreeOfAKind
    case TwoPair
    case OnePair
    case HighCard
  }

  implicit val cardCombinationOrdering: Ordering[CardCombination] =
    Ordering.by[CardCombination, Int] {
      case CardCombination.FiveOfAKind => 6
      case CardCombination.FourOfAKind => 5
      case CardCombination.FullHouse => 4
      case CardCombination.ThreeOfAKind => 3
      case CardCombination.TwoPair => 2
      case CardCombination.OnePair => 1
      case CardCombination.HighCard => 0
    }

  implicit val characterOrdering: Ordering[Char] =
    Ordering.by[Char, Int] {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 0
      case 'T' => 10
      case c => c.asDigit
    }

  private def mostFrequentCharacter(list: List[Char]): Char =
    list.groupBy(identity).view.mapValues(_.size).maxBy(_._2)._1

  case class CamelCard(card: List[Char], bid: Int) {
    def bestJokerReplacement: CamelCard = {
      @tailrec
      def go(index: Int, acc: CamelCard): CamelCard =
        if index == 5 then acc
        else {
          val bestHand: CamelCard =
            if this.card(index) == 'J' then acc.copy(card = acc.card.updated(index, mostFrequentCharacter(card.filterNot(_ == 'J'))))
            else acc
          go(index + 1, bestHand)
        }
      if this.cardCombination == CardCombination.FiveOfAKind then this else go(0, this)
    }

    def cardCombination: CardCombination = {
      val cardCounts = card.groupBy(identity).view.mapValues(_.size).toMap
      cardCounts.values.toList.sorted.reverse match {
        case List(5) => CardCombination.FiveOfAKind
        case List(4, 1) => CardCombination.FourOfAKind
        case List(3, 2) => CardCombination.FullHouse
        case 3 :: _ => CardCombination.ThreeOfAKind
        case 2 :: 2 :: _ => CardCombination.TwoPair
        case 2 :: _ => CardCombination.OnePair
        case _ => CardCombination.HighCard
      }
    }

    def isWinningCard(other: CamelCard): Boolean =
      if this.cardCombination == other.cardCombination then
        this.card.zip(other.card).dropWhile((a, b) => a == b).headOption match {
          case Some((a, b)) => characterOrdering.gt(a, b)
          case None => throw new Exception("Two identical hands")
        }
      else this.cardCombination > other.cardCombination

    def isWinningCardWithJokerReplacement(other: CamelCard): Boolean =
      if this.bestJokerReplacement.cardCombination == other.bestJokerReplacement.cardCombination then
        this.card.zip(other.card).dropWhile((a, b) => a == b).headOption match {
          case Some((a, b)) => characterOrdering.gt(a, b)
          case None => throw new Exception("Two identical hands")
        }
      else this.bestJokerReplacement.cardCombination > other.bestJokerReplacement.cardCombination
  }

  private def winningCard(cards: List[CamelCard], replaceJokers: Boolean): CamelCard =
    if replaceJokers then cards.reduce((a, b) => if a.isWinningCardWithJokerReplacement(b) then a else b)
    else cards.reduce((a, b) => if a.isWinningCard(b) then a else b)

  def solve(input: List[CamelCard], replaceJokers: Boolean): Long = {
    @tailrec
    def go(remainingCards: List[CamelCard], total: Long, rank: Long): Long =
      remainingCards match {
        case Nil => total
        case cards =>
          val winning = winningCard(cards, replaceJokers)
          println("Winning card: " + winning.card.mkString + " bid: " + winning.bid + " rank: " + rank)
          go(cards.filterNot(_ == winning), total + (winning.bid * rank), rank - 1)
      }
    go(input, 0, input.size)
  }

}
