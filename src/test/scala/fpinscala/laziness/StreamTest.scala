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

  "Exercise 5.11" should "implement unfold" in {
    Stream.unfold(1)(s => Some(s, 2)).take(3).toList shouldBe List(1, 2, 2)
  }

  "Exercise 5.12" should "implement fibs via unfold" in {
    Stream.fibsViaUnfold.take(5).toList shouldBe List(0, 1, 1, 2, 3)
  }

  it should "implement from via unfold" in {
    Stream.fromViaUnfold(1).take(3).toList shouldBe List(1, 2, 3)
  }

  it should "implement constant via unfold" in {
    Stream.constantViaUnfold(1).take(3).toList shouldBe List(1, 1, 1)
  }

  "Exercise 5.13" should "implement map via unfold" in {
    Stream(1, 2, 3).mapViaUnfold(_ + 1).toList shouldBe List(2, 3, 4)
  }

  it should "implement take via unfold" in {
    Stream(1, 2, 3).takeViaUnfold(2).toList shouldBe List(1, 2)
  }

  it should "implement takeWhile via unfold" in {
    Stream(1, 2, 3).takeWhileViaUnfold(_ < 3).toList shouldBe List(1, 2)
  }

  it should "implement zipWith" in {
    Stream(1, 2, 3).zipWith(Stream(1, 2, 3))((a, b) => a + b).toList shouldBe List(2, 4, 6)
  }

  it should "implement zipAll" in {
    Stream(1, 2).zipAll(Stream(1)).toList shouldBe List((Some(1), Some(1)), (Some(2), None))
  }

  "Exercise 5.14" should "implement startsWith" in {
    Stream(1,2,3) startsWith Stream(1,2) shouldBe true
  }

  "Exercise 5.15" should "implement tails" in {
    Stream(1,2).tails.toList.map(_.toList) shouldBe List(List(1,2), List(2), List())
  }

  "Exercise 5.16" should "implement scanRight" in {
    Stream(1,2,3).scanRight(0)(_ + _).toList shouldBe List(6,5,3,0)
  }

}
