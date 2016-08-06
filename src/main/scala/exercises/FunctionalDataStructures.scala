package exercises

sealed trait List[+A] {

  def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(x,xs) => Cons(f(x), xs.map(f) ) 
  }

  // exercise
  def filter(f: A => Boolean): List[A] = ???

  def foldRight[B](z: B, f: (A,B) => B): B = this match {
    case Nil        => z
    case Cons(x,xs) => f(x, xs.foldRight(z, f))
  }

}
case object Nil                             extends List[Nothing]
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

  def all(list: List[Boolean]): Boolean = list match {
    case Nil        => true
    case Cons(x,xs) => x && all(xs)
  }

  def totalLength(list: List[String]): Int = list match {
    case Nil        => 0
    case Cons(x,xs) => x.length + totalLength(xs)
  }

  def sum2(list: List[Int]): Int =
    ???

  def product2(list: List[Int]): Int =
    ???

  def all2(list: List[Boolean]): Boolean =
    ???

  def totalLength2(list: List[String]): Int =
    ???

  // Constructor varíadico
  def apply[A](as: A*): List[A] =
    if(as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def empty[A]: List[A] = Nil

}
