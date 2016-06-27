package exercises

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "List" should "apply should build a List" in {
    List(1,2,3) should equal (Cons(1,Cons(2,Cons(3,Nil))))
    List() should equal (Nil)
  }

  it should "compute the sum of a list of ints" in {
    List.sum( List(-3,6,2,1) ) should equal (6)
    List.sum( List() ) should equal (0)
  }

  it should "drop while a predicate is true" in {
    List.dropWhile( List(1, 2, 3, 4 ,5))( x => x < 4 ) should equal ( List(4, 5) )
    List.dropWhile( List(-1,1,2,3))( x => x > 0 ) should equal ( List(-1, 1, 2, 3) )
    List.dropWhile( List(1,1,2,1,3))( x => x == 1 ) should equal ( List(2, 1, 3) )
  }

  it should "compute the length of a list" in {
    List.length( List() ) should equal (0)
    List.length( List(1) ) should equal (1)
    List.length( List(4,6,7) ) should equal (3)
  }

  it should "reverse a list" in {
    List.reverse( List[Int]() ) should equal ( List[Int]() )
    List.reverse( List(1) ) should equal ( List(1) )
    List.reverse( List(1,2,3,4,5) ) should equal ( List(5,4,3,2,1) )
  }

  it should "add 1 to all elems of a list" in {
    List.addOne2All( List() ) should equal ( List() )
    List.addOne2All( List(4) ) should equal ( List(5) )
    List.addOne2All( List(6,2,8) ) should equal ( List(7,3,9) )
  }

  it should "convert all doubles to strings" in {
    List.allDoubles2Strings( List(1.0,3.4,6.7) ) should equal (List("1.0","3.4","6.7"))
  }

  it should "filter a list" in {
    List.filter( List(1,2,3,4,5,6) )( _%2 == 0 ) should equal ( List(2,4,6) )
  }

}
