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

  "Exercise 5.3" should "implement takeWhile" in {
    Stream(1, 2, 3).takeWhile(_ < 3).toList shouldBe Stream(1, 2).toList
  }

  "Exercise 5.4" should "implement forAll" in {
    Stream(1, 2, 3).forAll(_ < 2) shouldBe false
    Stream(1, 2, 3).forAll(_ > 0) shouldBe true
  }

  "Exercise 5.5" should "implement takeWhileViaFoldRight" in {
    Stream(1, 2, 3, 1).takeWhileViaFoldRight(_ < 3).toList shouldBe Stream(1, 2).toList
  }

  "Exercise 5.6" should "implement headOption" in {
    Stream(1, 2, 3).headOption shouldBe Some(1)
    Stream().headOption shouldBe None
  }

  "Exercise 5.7" should "implement map" in {
    Stream(1, 2, 3).map(_ + 1).toList shouldBe Stream(2, 3, 4).toList
  }

  it should "implement filter" in {
    Stream(1, 2, 3).filter(_ > 1).toList shouldBe Stream(2, 3).toList
  }

  it should "implement append" in {
    Stream(1, 2, 3).append(Stream(4, 5)).toList shouldBe Stream(1, 2, 3, 4, 5).toList
  }

  it should "implement flatMap" in {
    Stream(1, 2, 3).flatMap(Stream(_)).toList shouldBe Stream(1, 2, 3).toList
  }

  "Exercise 5.8" should "implement constant" in {
    Stream.constant(1).take(3).toList shouldBe List(1, 1, 1)
  }

  "Exercise 5.9" should "implement from" in {
    Stream.from(1).take(3).toList shouldBe List(1, 2, 3)
  }

  "Exercise 5.10" should "implement fibs" in {
    Stream.fibs.take(5).toList shouldBe List(0, 1, 1, 2, 3)
  }

}
