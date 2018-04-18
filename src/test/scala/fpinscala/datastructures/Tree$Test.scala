package fpinscala.datastructures

import org.scalatest.FunSuite

/**
  * Created by andy on 16/6/11.
  */
class Tree$Test extends FunSuite {

  test("testDepthWithFold") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    assert(Tree.depthWithFold(t) == 2)
  }

}
