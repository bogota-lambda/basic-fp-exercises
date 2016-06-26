package exercises

object FunctionalLoops {

  def nonTailRecFact(n: Int): Int = {
    if(n <= 0)
      1
    else
      n * nonTailRecFact(n-1)
  }

  def fact(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if(n<=0)
        acc
      else
        go(n-1, n*acc)
    go(n, 1)
  }

  def sumInRange(start: Int, end: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if(n>end)
        acc
      else
        go(n+1, n+acc)
    go(start, 0)
  }

}
