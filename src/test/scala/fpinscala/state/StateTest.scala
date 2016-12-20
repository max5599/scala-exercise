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

}
