package day9

import scala.annotation.tailrec

object MirageMaintenance {

  def readInput(input: String): List[Long] =
    input.split(" ").map(_.toLong).toList

  def getHistories(line: List[Long]): List[List[Long]] = {
    @tailrec
    def go(histories: List[List[Long]]): List[List[Long]] = histories.head match {
      case x if x.forall(_ == 0) => histories
      case x =>
        val differences = x.zip(x.tail).map { case (a, b) => b - a }
        go(differences :: histories)
    }
    go(List(line))
  }

  def extrapolateHistory(line: List[Long]): Long = {
    @tailrec
    def go(remainingHistories: List[List[Long]], previousLine: List[Long]): Long = remainingHistories match {
      case Nil => previousLine.last
      case history :: histories =>
        val last = history.last + previousLine.last
        go(histories, history.appended(last))
    }
    go(getHistories(line), List(0))
  }

  def extrapolateHistoryBackwards(line: List[Long]): Long = {
    @tailrec
    def go(remainingHistories: List[List[Long]], previousLine: List[Long]): Long = remainingHistories match {
      case Nil => previousLine.head
      case history :: histories =>
        val last = history.head - previousLine.head
        go(histories, history.prepended(last))
    }

    go(getHistories(line), List(0))
  }

}
