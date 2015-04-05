import scala.annotation.tailrec

// sealed means:
// all definitions must be contained in this file
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

  def sum(ints: List[Int]): Int = foldRight(ints, 0)((x, y) => x + y)

  def product(ds: List[Double]): Double = foldRight(ds, 1.0)((x, y) => x * y)

  // there is no early ternimation with this impl (e.g if encountered a 0)
  // we need to traverse the entire list
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(a, t) => f(a, foldRight(t, z)(f))
  }

  // 3.13 - stacksafe foldRight over foldLeft
  def foldRightFL[A, B](as: List[A], z: B)(f: => (A, B) => B) = {
    foldLeft[A,B](as,z)((b, a) => f(a, b))
  }

  def length[A](as: List[A]): Int = as match {
    case Nil => 0
    case Cons(a, t) => 1 + length(t)
  }

  def length2[A](as: List[A]): Int = foldRight(as, 0)((_, s) => 1 + s)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))
  // type mismatch: expected NotInferredA, actual: A
  // TODO why doesnt it work without explicit args here while it worked with foldRight?
  //  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])(Cons(_,_))

  def append[A](as: List[A], other: List[A]): List[A] = as match {
    case Nil => other
    case Cons(h, Nil) => Cons(h, other)
    case Cons(h, t) => Cons(h, append(t, other))
  }

//  def appendFold[A](as: List[A], other: List[A]): List[A] = foldLeft(as, other)((b, a) => Cons(b, a))
  def appendFold[A](as: List[A], other: List[A]): List[A] = foldRight(as, other)((a, b) => Cons(a, b))

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil:List[A])(appendFold)
  }
}
val l = List(1, 2, 3, 4)
val d = List(1.0, 2.0, 3.0, 4.0)
List.sum(l)
List.product(d)
// 'copy' the list by passing the Const ctor
List.foldRight(l, Nil: List[Int])(Cons(_, _))

List.length(l)
List.length2(l)

List.foldLeft(l, 0)(_ + _)
List.foldLeft(l, 1)(_ * _)

// 3.12 - reverse
List.reverse(l)

// 3.13 - implementing foldRight through foldLeft
List.foldRightFL(l, Nil: List[Int])(Cons(_, _))

val l2 = List(5, 6, 7, 8)
List.append(Nil, l2)
List.append(l, l2)
// 3.14
List.appendFold(l, l2)

val l3 = List(9, 10, 11)
val l4 = List(12, 13, 14)
List.concat(List(l, l2, l3, l4))

