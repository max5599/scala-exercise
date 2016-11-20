package fpinscala.datastructure

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {

  "Exercise 3.25" should "implement size" in {
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 5
    Tree.sizeViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 5
  }

  "Exercise 3.26" should "implement size" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    Tree.maximumViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
  }

  "Exercise 3.27" should "implement depth" in {
    Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 2
    Tree.depthViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 2
  }

  "Exercise 3.28" should "implement map" in {
    Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 1) shouldBe Branch(Leaf(2), Leaf(3))
    Tree.mapViaFold(Branch(Leaf(1), Leaf(2)))(_ + 1) shouldBe Branch(Leaf(2), Leaf(3))
  }

  "Exercise 3.29" should "implement fold" in {
    Tree.fold(Branch(Leaf(1), Leaf(2)))(a => a)(_ + _) shouldBe 3
  }

}
