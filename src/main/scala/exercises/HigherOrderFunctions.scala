package exercises

import FunctionalLoops._

object HigherOrderFunctions {

  def abs(x: Int): Int =
    if(x<0)
      -x
    else
      x

  def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatFact(x: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(x, fact(x))
  }

  def formatResult(name: String, x: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, x, f(x))
  }

  def formatAbs2(x: Int): String = formatResult("absolute value", x, abs)

  def formatFact2(x: Int): String = formatResult("factorial", x, fact)

  def findFirst(strings: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if(n >= strings.length)
        -1
      else if(strings(n) == key)
        n
      else
        loop(n+1)
    loop(0)
  }

  def findFirst[A](array: Array[A], predicate: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
      if(n >= array.length)
        -1
      else if(predicate( array(n) ))
        n
      else
        loop(n+1)
    loop(0)
  }

  def isSorted[A](array: Array[A], ordered: (A,A) => Boolean ): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if(n >= array.length || n == array.length - 1)
        true
      else if(!ordered(array(n), array(n+1)))
        false
      else
        loop(n+1)
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFact(7))
  }

}
