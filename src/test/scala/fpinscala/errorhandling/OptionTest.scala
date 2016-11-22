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

}
