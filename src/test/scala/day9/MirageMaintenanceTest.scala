package day9

import org.scalatest.wordspec.AnyWordSpec

class MirageMaintenanceTest extends AnyWordSpec {

  val input: List[List[Long]] =
    general.ReadInTestData.getPuzzleInput(MirageMaintenance.readInput)("src/test/scala/day9/testData.txt")

  "Mirage Maintenance" should {
    "get histories" in {
      assert(
        MirageMaintenance.getHistories(List(0, 3, 6, 9, 12, 15)) ==
          List(List(0, 0, 0, 0), List(3, 3, 3, 3, 3), List(0, 3, 6, 9, 12, 15))
      )
    }
    
    "extrapolate histories" in {
      assert(MirageMaintenance.extrapolateHistory(List(0, 3, 6, 9, 12, 15)) == 18)
      assert(MirageMaintenance.extrapolateHistory(List(1, 3, 6, 10, 15, 21)) == 28)
      assert(MirageMaintenance.extrapolateHistory(List(10, 13, 16, 21, 30, 45)) == 68)
    }

    "sum extrapolated histories" in {
      assert(input.map(MirageMaintenance.extrapolateHistory).sum == 114)
    }

    "extrapolate previous backward histories" in {
      assert(MirageMaintenance.extrapolateHistoryBackwards(List(0, 3, 6, 9, 12, 15)) == -3)
      assert(MirageMaintenance.extrapolateHistoryBackwards(List(1, 3, 6, 10, 15, 21)) == 0)
      assert(MirageMaintenance.extrapolateHistoryBackwards(List(10, 13, 16, 21, 30, 45)) == 5)
    }

    "sum extrapolated backward histories" in {
      assert(input.map(MirageMaintenance.extrapolateHistoryBackwards).sum == 2)
    }
  }

}
