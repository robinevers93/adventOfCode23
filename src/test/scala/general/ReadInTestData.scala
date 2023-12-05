package general

import scala.io.{BufferedSource, Source}

object ReadInTestData {

  def getPuzzleInput[T](transform: String => T)(filename: String): List[T] = {
    val bufferedSource: BufferedSource = Source.fromFile(filename)
    val puzzleAsList: List[T] = bufferedSource.getLines.toList.map(transform)
    bufferedSource.close
    puzzleAsList
  }

  def getPuzzleInputAsVector[T](transform: String => T)(filename: String): Vector[T] = {
    val bufferedSource: BufferedSource = Source.fromFile(filename)
    val puzzleAsList: Vector[T] = bufferedSource.getLines.toVector.map(transform)
    bufferedSource.close
    puzzleAsList
  }

}
