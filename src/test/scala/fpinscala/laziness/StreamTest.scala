package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  "Exercise 5.1" should "implement toList" in {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  "Exercise 5.2" should "implement take" in {
    Stream(1, 2, 3).take(2).toList shouldBe Stream(1, 2).toList
  }

  it should "implement drop" in {
    Stream(1, 2, 3).drop(1).toList shouldBe Stream(2, 3).toList
  }

  "Exercise 5.3" should "implement taleWhile" in {
    Stream(1, 2, 3).takeWhile(_ < 3).toList shouldBe Stream(1, 2).toList
  }

}
