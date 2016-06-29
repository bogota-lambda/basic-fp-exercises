package exercises

sealed trait List[+A]
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

  // exercise 
  def drop[A](list: List[A], n: Int): List[A] =
    if(n <= 0) {
      list
    } else {
      list match {
        case Nil        => Nil
        case Cons(x,xs) => drop(xs, n-1)
      }
    }

  // exercise
  def dropWhile[A](list: List[A])( p: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x,xs) =>
      if(p(x))
        dropWhile(xs)(p)
      else
        list
  }

  def foldRight[A,B](list: List[A], z: B)(f: (A,B) => B): B = list match {
    case Nil        => z
    case Cons(x,xs) => f( x, foldRight(xs,z)(f) )
  }

  def sum2(list: List[Int]): Int = foldRight(list, 0)(_ + _)

  def product2(list: List[Int]): Int = foldRight(list, 1)(_ * _)

  // exercise
  def length[A](list: List[A]): Int = foldRight(list,0)( (_,acc) => acc+1 )

  // exercise
  def foldLeft[A,B](list: List[A], z: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def loop(list: List[A], acc: B): B = list match {
      case Nil         => acc
      case Cons(x, xs) => loop(xs, f(acc,x))
    }
    loop(list, z)
  }

  // exercise
  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]())( (tail, x) => Cons(x,tail) )

  // exercise
  def addOne2All(list: List[Int]): List[Int] = list match {
    case Nil        => Nil
    case Cons(x,xs) => Cons(x+1, addOne2All(xs))
  }

  // exercise
  def allDoubles2Strings(list: List[Double]): List[String] = list match {
    case Nil        => Nil
    case Cons(x,xs) => Cons(x.toString, allDoubles2Strings(xs))
  }

  // exercise
  def map[A,B](list: List[A])(f: A => B): List[B] = list match {
    case Nil        => Nil
    case Cons(x,xs) => Cons(f(x), map(list)(f))
  }

  // exercise
  def filter[A](list: List[A])(f: A => Boolean): List[A] = foldRight(list, empty[A])( (x,xs) => if(f(x)) Cons(x,xs) else xs )

  // Constructor varíadico
  def apply[A](as: A*): List[A] =
    if(as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def empty[A]: List[A] = Nil

}
