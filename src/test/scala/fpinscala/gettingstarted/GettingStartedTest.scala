package fpinscala.gettingstarted

import org.scalatest.FunSuite

class GettingStartedTest extends FunSuite{

  test("PolymorphicFunctions.isSorted") {
    val src = Array[String]("abc", "bcd", "cde", "xyz")
    assert(PolymorphicFunctions.isSorted(src, (a: String, b: String) => a > b))
  }
}
