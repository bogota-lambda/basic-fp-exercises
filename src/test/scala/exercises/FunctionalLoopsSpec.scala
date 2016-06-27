package exercises

import org.scalatest._
import FunctionalLoops._

class FunctionalLoopsSpec extends FlatSpec with Matchers {
  "FunctionalLoops" should "sum in range (Non tail recursive version)" in {
    sumInRangeNTR(1, 2) should equal (3)
    sumInRangeNTR(1, 10) should equal (55)
    sumInRangeNTR(6,6) should equal (6)
    sumInRangeNTR(4,2) should equal (0)
  }

  it should "sum in range (Tail recursive version)" in {
    sumInRange(1, 2) should equal (3)
    sumInRange(1, 10) should equal (55)
    sumInRange(6,6) should equal (6)
    sumInRange(4,2) should equal (0)
  }
}
