package day2

object Cubes {

  case class Round(red: Int, green: Int, blue: Int)
  case class Game(id: Int, rounds: List[Round])

  private def readRound(str: String): Round = {
    val colours = str.split(", ").toList
    val blues =
      colours.find(_.contains("blue")).map(_.dropRight(5).toInt).getOrElse(0)
    val greens =
      colours.find(_.contains("green")).map(_.dropRight(6).toInt).getOrElse(0)
    val reds =
      colours.find(_.contains("red")).map(_.dropRight(4).toInt).getOrElse(0)
    Round(reds, greens, blues)
  }

  private def readRounds(roundsInfo: String): List[Round] =
    roundsInfo.split("; ").map(readRound).toList

  def readGame(gameString: String): Game = gameString.split(": ").toList match {
    case id :: roundsInfo => Game(id.drop(5).toInt, readRounds(roundsInfo.head))
    case _ => throw new IllegalArgumentException("Invalid game string")
  }

  def minimumRequiredCubes(games: List[Game]): List[Int] = games.map { game =>
    val redsUsed = game.rounds.map(_.red).max
    val greensUsed = game.rounds.map(_.green).max
    val bluesUsed = game.rounds.map(_.blue).max
    redsUsed * greensUsed * bluesUsed
  }

  def possibleGames(games: List[Game], totalRed: Int, totalGreen: Int, totalBlue: Int): List[Game] =
    games.filter { game =>
      val redsUsed = game.rounds.map(_.red).max
      val greensUsed = game.rounds.map(_.green).max
      val bluesUsed = game.rounds.map(_.blue).max
      redsUsed <= totalRed && greensUsed <= totalGreen && bluesUsed <= totalBlue
    }

  def sumIds(games: List[Game]): Int = games.map(_.id).sum

}
