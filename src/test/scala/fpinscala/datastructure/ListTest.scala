package fpinscala.datastructure

import fpinscala.datastructure.List._
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class ListTest extends FlatSpec with Matchers {

  "Exercise 3.1" should "" in {
    val res = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    res shouldBe 3
  }

  "Exercise 3.2" should "implement tail" in {
    tail(List(1, 2, 3)) shouldBe Some(List(2, 3))
    tail(List(1)) shouldBe Some(List())
    tail(List()) shouldBe empty
  }

  "Exercise 3.3" should "implement setHead" in {
    setHead(List(2, 2), 1) shouldBe List(1, 2)
    setHead(List(), 1) shouldBe List()
  }

  "Exercise 3.4" should "implement drop" in {
    drop(List(1, 2, 3), 1) shouldBe List(2, 3)
    drop(List(), 4) shouldBe List()
  }

  "Exercise 3.5" should "implement drop while" in {
    dropWhile(List(1, 2, 3))(x => x <= 1) shouldBe List(2, 3)
  }

  "Exercise 3.6" should "implement init" in {
    init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  "Exercise 3.8" should "test con and nil" in {
    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  }

  "Exercise 3.9" should "implement length" in {
    List.length(List(1, 2, 3)) shouldBe 3
  }

  "Exercise 3.10" should "implement foldLeft" in {
    foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  "Exercise 3.11" should "use foldLeft for sum, product and length" in {
    sum3(List(1, 2)) shouldBe 3
    product3(List(1, 2)) shouldBe 2
    length3(List(1, 2, 3, 4)) shouldBe 4
  }

  "Exercise 3.12" should "implements reverse" in {
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  "Exercise 3.13" should "implements foldLeft using foldRight" in {
    foldLeft(List(1, 2), List(3, 4))((acc, a) => Cons(a, acc)) shouldBe List(2, 1, 3, 4)
    foldLeftViaFoldRight(List(1, 2), List(3, 4))((acc, x) => Cons(x, acc)) shouldBe List(2, 1, 3, 4)
  }

  "Exercise 3.13" should "implements foldRight using foldLeft" in {
    foldRight(List(1, 2), List(3))((a, acc) => Cons(a, acc)) shouldBe List(1, 2, 3)
    foldRightUsingFoldLeft(List(1, 2), List(3))(Cons(_, _)) shouldBe List(1, 2, 3)
  }

  "Exercise 3.14" should "implements append" in {
    append(List(1, 2), List(3)) shouldBe List(1, 2, 3)
  }

  "Exercise 3.15" should "implements flatten" in {
    flatten(List(List(1, 2), List(3))) shouldBe List(1, 2, 3)
  }
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 1)

  def length3[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => x + 1)

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def sum3(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def product3(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def tail[A](list: List[A]): Option[List[A]] = list match {
    case Nil => None
    case Cons(_, xs) => Some(xs)
  }

  def setHead[A](list: List[A], head: A) = list match {
    case Nil => Nil
    case Cons(_, xs) => Cons(head, xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}