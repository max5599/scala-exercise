package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {

  "Exercise 4.1" should "implement map" in {
    Some(1).map(_ + 1) shouldBe Some(2)
    None.map(_ => "WTF") shouldBe None
  }

  it should "implement getOrElse" in {
    Some(1).getOrElse(0) shouldBe 1
    None.getOrElse("WTF") shouldBe "WTF"
  }

  it should "implement flatMap" in {
    Some(1).flatMap(a => Some(a)) shouldBe Some(1)
    None.flatMap(_ => Some("WTF")) shouldBe None
    Some(1).flatMap(_ => None) shouldBe None
  }

  it should "implement orElse" in {
    Some(1).orElse(Some(2)) shouldBe Some(1)
    None.orElse(Some(2)) shouldBe Some(2)
  }

  it should "implement filter" in {
    Some(1).filter(_ > 1) shouldBe None
    Some(1).filter(_ == 1) shouldBe Some(1)
    None.filter(_ => true) shouldBe None
  }

  "Exercise 4.2" should "implement variance" in {
    Option.variance(Seq(2.0)) shouldBe Some(0.0)
    Option.variance(Seq(1.0, 2.0)) shouldBe Some(0.25)
  }

  "Exercise 4.3" should "implement map2" in {
    Option.map2(Some(1), Some("a"))((a, b) => s"$a + $b") shouldBe Some("1 + a")
    Option.map2(None, Some("a"))((a, b) => s"$a + $b") shouldBe None
    Option.map2(Some(1), None)((a, b) => s"$a + $b") shouldBe None
  }

  "Exercise 4.4" should "implement sequence" in {
    Option.sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
    Option.sequence(List(Some(1), None)) shouldBe None
  }

  "Exercise 4.5" should "implement traverse" in {
    Option.traverse(List(1, 2))(a => Some(a + 1)) shouldBe Some(List(2, 3))
    Option.traverse(List(1, 2))(a => if (a == 1) Some(a) else None) shouldBe None
  }

}
