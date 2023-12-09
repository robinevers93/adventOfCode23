package day8

import scala.annotation.tailrec
import general.MathStuff

object Wasteland {

  enum Direction:
    case Left, Right

  case class Network(instructions: List[Direction], nodeMap: Map[(String, Direction), String])

  def readInput(input: List[String]): Network = {
    val instructions = input.head.map(c => if c == 'L' then Direction.Left else Direction.Right).toList
    val nodeMap = input.tail.tail
      .map { line =>
        val pattern = "([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)".r
        line match {
          case pattern(from, to1, to2) => Map(((from, Direction.Left) -> to1), ((from, Direction.Right), to2))
          case _ => throw new RuntimeException(s"Invalid input: $line")
        }
      }
      .foldLeft(Map.empty[(String, Direction), String]) { (acc, map) => acc ++ map }
    Network(instructions, nodeMap)
  }

  private def followInstruction(currentLocation: String, nodeMap: Map[(String, Direction), String], instruction: Direction): String =
    nodeMap(currentLocation, instruction)

  def solve1(network: Network): Int = {
    @tailrec
    def go(remainingInstructions: List[Direction], currentLocation: String, numberOfSteps: Int): Int = {
      if currentLocation == "ZZZ" then numberOfSteps
      else if remainingInstructions.isEmpty then go(network.instructions, currentLocation, numberOfSteps)
      else go(remainingInstructions.tail, followInstruction(currentLocation, network.nodeMap, remainingInstructions.head), numberOfSteps + 1)
    }
    go(network.instructions, "AAA", 0)
  }

  private def getStartingPositions(network: Network): List[String] =
    network.nodeMap.keys.filter(_._1.endsWith("A")).map(_._1).toList

  def solve2(network: Network): Long = {
    @tailrec
    def go(remainingInstructions: List[Direction], currentLocation: String, numberOfSteps: Long): Long = {
      if currentLocation.endsWith("Z") then numberOfSteps
      else if remainingInstructions.isEmpty then go(network.instructions, currentLocation, numberOfSteps)
      else go(remainingInstructions.tail, followInstruction(currentLocation, network.nodeMap, remainingInstructions.head), numberOfSteps + 1)
    }

    val startingPositions = getStartingPositions(network)
    println(s"startingPositions are: $startingPositions")
    val stepsForStartingPositions = startingPositions.map(startingPositon => go(network.instructions, startingPositon, 0))
    MathStuff.leastCommonMultiple(stepsForStartingPositions)
  }

}
