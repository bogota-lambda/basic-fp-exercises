package exercises

object FunctionalLoops {

  def nonTailRecFact(n: Int): Int = {
    if(n <= 1)
      1
    else
      n * nonTailRecFact(n-1)
  }

  def fact(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if(n <= 1)
        acc
      else
        go(n-1, n*acc)
    go(n, 1)
  }

  // exercise
  def sumInRangeNTR(start: Int, end: Int): Int =
    if(start > end)
      0
    else
      start + sumInRangeNTR(start+1, end)

  // exercise
  def sumInRange(start: Int, end: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if(n>end)
        acc
      else
        go(n+1, n+acc)
    go(start, 0)
  }

}
