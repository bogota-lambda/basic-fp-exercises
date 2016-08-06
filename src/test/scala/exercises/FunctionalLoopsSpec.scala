package exercises

import org.scalatest._
import FunctionalLoops._

class FunctionalLoopsSpec extends FlatSpec with Matchers {
  "FunctionalLoops" should "sum in range" in {
    sumInRange(1, 2) should equal (3)
    sumInRange(1, 10) should equal (55)
    sumInRange(6,6) should equal (6)
    sumInRange(4,2) should equal (0)
  }
}
