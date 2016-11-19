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

  "Exercise 3.16" should "add 1 to each elements" in {
    increment(List(1, 2)) shouldBe List(2, 3)
  }

  "Exercise 3.17" should "convert double to string" in {
    double2String(List(1.0, 2.0)) shouldBe List("1.0", "2.0")
  }

  "Exercise 3.18" should "implement map" in {
    map(List(1.0, 2.0))(_.toString) shouldBe List("1.0", "2.0")
  }

  "Exercise 3.19" should "implement filter" in {
    filter(List(1, 2, 3))(_ > 1) shouldBe List(2, 3)
  }

  "Exercise 3.20" should "implement flatmap" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "Exercise 3.21" should "implement filterWithFlatMap" in {
    filter(List(1, 2, 3))(_ > 1) shouldBe List(2, 3)
  }

  "Exercise 3.22" should "implement addPairwise" in {
    addPairwise(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  "Exercise 3.23" should "implement zipWith" in {
    zipWith(List("1", "2", "3"), List("4", "5", "6"))((a, b) => s"$a$b") shouldBe List("14", "25", "36")
  }

  "Exercise 3.24" should "implement hasSubsequence" in {
    hasSubsequence(List(1,2,3), List(2,3)) shouldBe true
    hasSubsequence(List(1,2,3), List(2,3,4)) shouldBe false
  }
}

object List {

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(e => if (f(e)) List(e) else Nil)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def double2String(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def increment(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
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

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]