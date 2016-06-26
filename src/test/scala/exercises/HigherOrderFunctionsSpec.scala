package exercises

import org.scalatest._
import HigherOrderFunctions._

class HigherOrderFunctionsSpec extends FlatSpec with Matchers {
  "HigherOrderFunctions" should "format absolute values" in {
    formatAbs(-34) should equal ("The absolute value of -34 is 34")
    formatAbs(3) should equal ("The absolute value of 3 is 3")
    formatAbs2(-42) should equal (formatAbs(-42))
  }

  it should "find a key in a string array" in {
    findFirst(Array("hola","casa","sol"), "casa") should equal (1)
    findFirst(Array("hola","casa","sol"), "carro") should equal (-1)
  }
}
