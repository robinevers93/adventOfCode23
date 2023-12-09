package general

object MathStuff {

  def greatestCommonDivisor(l: Long, l1: Long): Long = {
    if l == 0 then l1
    else if l1 == 0 then l
    else if l > l1 then greatestCommonDivisor(l % l1, l1)
    else greatestCommonDivisor(l, l1 % l)
  }

  def leastCommonMultiple(a: Long, b: Long): Long = {
    val gcd = greatestCommonDivisor(a, b)
    (a * b) / gcd
  }

  def leastCommonMultiple(l: List[Long]): Long =
    l.reduce(leastCommonMultiple)

}
