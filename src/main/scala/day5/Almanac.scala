package day5

import zio.stream.{ZSink, ZStream}
import zio.{Runtime, Unsafe, ZIO}

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Almanac {

  case class SingleMap(destinationStart: Long, sourceStart: Long, range: Long) {
    def inSource(long: Long): Boolean = sourceStart <= long && long < sourceStart + range
    def toDestination(long: Long): Long = destinationStart + (long - sourceStart)
  }

  case class CombinedMap(maps: List[SingleMap]) {
    def follow(long: Long): Long =
      maps
        .find(_.inSource(long))
        .map(_.toDestination(long))
        .getOrElse(long)
  }

  case class SimpleAlmanac(seeds: List[Long], maps: List[CombinedMap])
  case class RangeAlmanac(seedRanges: List[NumericRange.Exclusive[Long]], maps: List[CombinedMap])

  def readMap(lines: List[String]): CombinedMap =
    val pattern = """(\d+) (\d+) (\d+)""".r
    CombinedMap(
      lines.map { case pattern(destinationStart, sourceStart, range) =>
        SingleMap(destinationStart.toLong, sourceStart.toLong, range.toLong)
      }
    )

  private def readMaps(input: List[String]): List[CombinedMap] = {
    @tailrec
    def readMaps(input: List[String], acc: List[CombinedMap]): List[CombinedMap] = {
      input match {
        case Nil => acc
        case _ :: tail =>
          val mapStrings = tail.takeWhile(_.nonEmpty)
          readMaps(tail.drop(mapStrings.size + 1), acc :+ readMap(mapStrings))
      }
    }
    readMaps(input, Nil)
  }

  def readAlmanac(input: List[String]): RangeAlmanac =
    RangeAlmanac(
      input.head
        .drop(7)
        .split(" ")
        .map(_.toLong)
        .toList
        .grouped(2)
        .map {
          case List(a, b) => a until a + b
          case _ => throw new RuntimeException("Invalid range")
        }
        .toList,
      readMaps(input.tail.tail)
    )

  def readSimpleAlmanac(input: List[String]): SimpleAlmanac =
    SimpleAlmanac(
      input.head.drop(7).split(" ").map(_.toLong).toList,
      readMaps(input.tail.tail)
    )

  private val largestLong: Long = 9223372036854775807L

  def solve1(input: SimpleAlmanac): Long = {
    val (seeds, maps) = (input.seeds, input.maps)

    def runMaps(seed: Long, maps: List[CombinedMap]): Long =
      maps.foldLeft(seed)((acc, f) => f.follow(acc))

    val (seed, minimum) = ZStream
      .fromIterable(seeds)
      .mapZIOParUnordered(50)(seed => ZIO.succeed((seed, runMaps(seed, maps))))
      .run(ZSink.foldLeft((0L, largestLong))((acc, output) => List(output, acc).minBy(_._2)))
      .unsafeRun

    println(s"minimum : $minimum, on seed $seed")
    minimum
  }

  def solve2(input: RangeAlmanac): Long = {
    val (seedRanges, maps) = (input.seedRanges, input.maps)

    def runMaps(seed: Long, maps: List[CombinedMap]): Long =
      maps.foldLeft(seed)((acc, f) => f.follow(acc))

    seedRanges
      .map(range =>
        val minForRange =
          ZStream
            .fromIterable(range)
            // .filter(_ % 10000 == 0)
            .mapZIOParUnordered(50)(seed => ZIO.succeed((seed, runMaps(seed, maps))))
            .run(ZSink.foldLeft((0L, largestLong))((acc, output) => List(output, acc).minBy(_._2)))
            .unsafeRun

        println(s"min for range: ${minForRange._2}, on seed ${minForRange._1}")
        minForRange._2
      )
      .min
  }

  extension [E, A](task: ZIO[Any, E, A]) {
    private def unsafeRun: A = Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(task).getOrThrowFiberFailure()
    }
  }
}
