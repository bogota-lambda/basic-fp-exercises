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

}
