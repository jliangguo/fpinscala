package fpinscala.datastructures

import org.scalatest.FunSuite

/**
  * Created by andy on 16/6/11.
  */
class List$Test extends FunSuite {

  test("testDropWhile") {
    val result = List.dropWhile(List(1, 2, 3, 2, 4, 2), (x: Int) => x == 1)
    assert(List(2,3,2,4,2) == result)
  }

  test("testInit") {
    val result = List.init(List(1,2,3,4))
    assert(List(1,2,3) == result)
  }

  test("testFoldRight") {
    val result = List.foldRight(List(1,2,3,4), Nil:List[Int])(Cons(_,_))
    println(result)
  }

  test("testAppendViaFoldLeft") {
    val result = List.appendViaFoldLeft(List(1,2), List(3,4))
    assert(List(1,2,3,4) == result)
  }

  test("testAppendViaFoldLeft2") {
    val result = List.appendViaFoldLeft2(List(1,2), List(3,4))
    assert(List(1,2,3,4) == result)
  }
}
