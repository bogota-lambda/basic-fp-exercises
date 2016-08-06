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

  it should "filter a list" in {
    List(1,2,3,4,5,6).filter( _%2 == 0 ) should equal ( List(2,4,6) )
    List(1).filter( _%2 == 0 ) should equal ( List() )
  }

  it should "sum elements in a list" in {
    List.sum2( List(4,10,2) ) should equal (16)
    List.sum2( List() ) should equal (0)
  }

}
