package exercises

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(list: List[Int]): Int = list match {
    case Nil        => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(list: List[Int]): Int = list match {
    case Nil        => 1
    case Cons(0,_)  => 0
    case Cons(x,xs) => x * product(xs)
  }

  // exercise 
  def drop[A](l: List[A], n: Int): List[A] =
    if(n <= 0) {
      l
    } else {
      l match {
        case Nil        => Nil
        case Cons(x,xs) => drop(xs, n-1)
      }
    }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) =>
      if(p(x))
        dropWhile(xs,p)
      else
        l
  }

  // Constructor varíadico
  def apply[A](as: A*): List[A] =
    if(as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

}
