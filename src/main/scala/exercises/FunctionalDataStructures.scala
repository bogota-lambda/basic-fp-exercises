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

  // exercise
  def dropWhile[A](l: List[A])( p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) =>
      if(p(x))
        dropWhile(xs)(p)
      else
        l
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil        => z
    case Cons(x,xs) => f( x, foldRight(xs,z)(f) )
  }

  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def product2(l: List[Int]): Int = foldRight(l, 1)(_ * _)

  // exercise
  def length[A](l: List[A]): Int = foldRight(l,0)( (_,acc) => acc+1 )

  // exercise
  def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def loop(l: List[A], acc: B): B = l match {
      case Nil         => acc
      case Cons(x, xs) => loop(xs, f(acc,x))
    }
    loop(l, z)
  }

  // exercise
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())( (tail, x) => Cons(x,tail) )

  // exercise
  def addOne2All(l: List[Int]): List[Int] = l match {
    case Nil        => Nil
    case Cons(x,xs) => Cons(x+1, addOne2All(xs))
  }

  // exercise
  def allDoubles2Strings(l: List[Double]): List[String] = l match {
    case Nil        => Nil
    case Cons(x,xs) => Cons(x.toString, allDoubles2Strings(xs))
  }

  // exercise
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil        => Nil
    case Cons(x,xs) => Cons(f(x), map(l)(f))
  }

  // exercise
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, empty[A])( (x,xs) => if(f(x)) Cons(x,xs) else xs )

  // Constructor varíadico
  def apply[A](as: A*): List[A] =
    if(as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def empty[A]: List[A] = Nil

}
