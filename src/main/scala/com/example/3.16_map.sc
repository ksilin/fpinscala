import fpinscala.datastructures.Cons

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // variadic ctor (see splat op)
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(a, t) => f(a, foldRight(t, z)(f))
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //3.16
  def inc(as: List[Int]): List[Int] =
    List.foldRight(as, Nil: List[Int])((a: Int, b: List[Int])=> Cons((a + 1), b))

  //3.17
  def toStr(as: List[Double]): List[String] =
    List.foldRight(as, Nil: List[String])((a: Double, b: List[String]) => Cons(a.toString, b))
//3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, Nil: List[B])((a: A, b: List[B]) => Cons(f(a), b))

  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a))Cons(a, b) else b)

  // helper from former examples
  def appendFold[A](as: List[A], other: List[A]): List[A] = foldRight(as, other)((a, b) => Cons(a, b))

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val mapAndAppend: (A, List[B]) => List[B] = (a: A, b: List[B]) => appendFold(f(a): List[B], b)
    List.foldRight(as, Nil: List[B])(mapAndAppend)
  }

  def filterFlat[A](as: List[A])(f: A => Boolean): List[A] = {
    def g(a: A): List[A] = if(f(a)) Cons(a, Nil) else Nil
    flatMap(as)(g)
  }

  // yay! I actually implemented the function very similarly to the book version
  // the Nil guard is probably unnecessary, we are matching for Cons anyway
  def addPairwise(as: List[Int], other: List[Int]): List[Int] = (as, other) match {
//    case (Cons(a, Nil), Cons(b, Nil)) => Cons(a + b, Nil)
    case (Cons(a, at), Cons(b, bt)) => Cons(a + b, addPairwise(at, bt))
    case _ => Nil
  }

  def zipWith[A, B, C](as: List[A], other: List[B])(f: (A, B) => C): List[C] = (as, other) match {
    case (Cons(a, at), Cons(b, bt)) => Cons(f(a, b), zipWith(at, bt)(f))
    case _ => Nil
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val origSub = sub

    def subGo(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(a, at), Cons(b, bt)) => if (a == b) subGo(at, bt) else subGo(at, origSub)
      }
    }
    subGo(sup, sub)
  }

  // the below book impl uses the elegant decomposition of the function in two
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence2(t, sub)
  }

}
val l = List(1, 2, 3, 4)
val d = List(1.0, 2.0, 3.0, 4.0)
// 'copy' the list by passing the Const ctor
List.foldRight(l, Nil: List[Int])(Cons(_, _))
// 3.16 - increase by 1
List.inc(l)
// 3.17 - map to string
List.toStr(d)
// 3.18 - map
List.map(l)(_+1)
List.map(d)(_.toString)
//3.19 - filter
List.filter(l)(_ > 2)
//3.20
List.flatMap(List(1,2,3))(i => List(i,i))
//3.21
List.filterFlat(l)(_ > 2)
val l2 = List(8, 7, 6, 5)
List.addPairwise(l, l2)
// the result contains as many elements as the shorter list
// this is also the behaviour of the regular zip function
val l3: List[Int] = List(1, 2)
List.addPairwise(l, l3)

List.zipWith(l, l2)(_+_)
List.zipWith(l, l3)(_+_)

List.hasSubsequence(l, l3) // true
List.hasSubsequence(l, l2) // false
List.hasSubsequence(List(1, 2, 3, 5, 3, 4), List(1, 2, 3,4)) // false
List.hasSubsequence2(List(1, 2, 3, 5, 3, 4), List(1, 2, 3,4)) // false
// TODO - test the need for storing the original subsequence for 'rewinding', since the book impl does not rewind
