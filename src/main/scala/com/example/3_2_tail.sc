// sealed means:
// all definitions must be contained in this file
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // variadic ctor function (see splat op)
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    // authors of FP in scala recommend throing an exception since calling tail on
    // an empty list can be a bug. sys.error("tail of empty list")
    /// at the moment at least, I see it differently - for me it is a abort condition
    case Nil => Nil
    case Cons(_, t) => t
  }

  //  3.3
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Cons(_, t) => Cons(newHead, t)
    case Nil => Cons(newHead, Nil)
  }

  // 3.4
  def drop[A](as: List[A], toDrop: Int): List[A] =
    if (toDrop <= 0) as
    else drop(tail(as), toDrop - 1)

  // 3.5
  def dropWhile[A](as: List[A])(condition: A => Boolean): List[A] = as match {
    case Nil => Nil
    case x@Cons(a, t) => if (condition(a)) dropWhile(t)(condition) else x
  }

  def init[A](as: List[A]): List[A] = as match{
      case Nil => Nil
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, init(t))
  }

}

val l = List(1, 2, 3, 4)

l match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case _ => 101
}

List.tail(l)
List.tail(Nil)

//3.3
List.setHead(l, 123)
List.setHead(Nil, 123)

// 3.4
List.drop(l, 1)
List.drop(l, 2)
List.drop(Nil, 2)

// 3.5
// if we do not modify the signature to improve type inference, we must explicitly
// give the param `i` of the condition a type
//List.dropWhile(l, _ < 4) type mismatch: expected NotInferredA => Boolean, actual Nothing => Any
//List.dropWhile(l, (i: Int) => i < 4)

// after havin improved the inference, we can use it without explicit types (it's a suboptimal impl of the scala compiler)
// haskell is able to infer such types
List.dropWhile(l)(i => i < 4)

// 3.6
List.init(l)
