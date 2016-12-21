package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class StateTest extends FlatSpec with Matchers {

  "Exercise 6.1" should "implement nonNegativeInt" in {
    RNG.nonNegativeInt(Simple(12))._1 should be >= 0
  }

  "Exercise 6.2" should "implement double" in {
    RNG.double(Simple(65))._1 should be >= 0.0
    RNG.double(Simple(65))._1 should be < 1.0
  }

  "Exercise 6.3" should "implement intDouble, doubleInt and double3" in {
    println("Exercise 6.3")
    println(RNG.intDouble(Simple(65)))
    println(RNG.doubleInt(Simple(65)))
    println(RNG.double3(Simple(65)))
  }

  "Exercise 6.4" should "implement intDouble, doubleInt and double3" in {
    println("Exercise 6.4")
    println(RNG.ints(4)(Simple(65)))
    println(RNG.ints2(5)(Simple(65)))
  }

  "Exercise 6.5" should "implement double via map" in {
    println("Exercise 6.5")
    println(RNG.doubleViaMap(Simple(65)))
  }

  "Exercise 6.6" should "implement map2" in {
    println("Exercise 6.6")
    println(RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)(_ + _)(Simple(65)))
  }

  "Exercise 6.7" should "implement sequence" in {
    println("Exercise 6.7")
    println(RNG.sequence(List(RNG.nonNegativeInt(_), RNG.nonNegativeInt(_)))(Simple(65)))
  }

}
