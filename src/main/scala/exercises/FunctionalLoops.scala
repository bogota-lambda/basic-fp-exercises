package exercises

object FunctionalLoops {

  def nonTailRecFact(n: Int): BigInt = {
    if(n <= 1)
      1
    else
      n * nonTailRecFact(n-1)
  }

  def fact(n: Int): BigInt = {
    @annotation.tailrec
    def go(n: Int, acc: BigInt): BigInt =
      if(n <= 1)
        acc
      else
        go(n-1, n*acc)
    go(n, 1)
  }

  // exercise
  def sumInRange(start: Int, end: Int): Int = {
    ???
  }

}
