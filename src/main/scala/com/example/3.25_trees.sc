sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  // 3.25
  def size[A](t: Tree[A]):Int  = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (1 + size(l) + size(r))
  }

  def maximum(t: Tree[Int]): Int  = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int  = t match {
    case Leaf(v) => 0
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B]  = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29 - I was stuck fo a while here because I did not realize that we would
  // need a combining function for Bs (and no initial value of B)
  def fold[A, B](t: Tree[A])(fb: A => B)(fc: (B, B) => B): B  = t match {
    case Leaf(v) => fb(v)
    case Branch(l, r) => fc(fold(l)(fb)(fc),fold(r)(fb)(fc))
  }

  def sizeFold[A](t: Tree[A]):Int = fold(t)(_ => 1)(_ + _ + 1)

  def maxFold(t: Tree[Int]):Int = fold(t)(identity)(_ max _)

  def depthFold[A](t: Tree[A]):Int = fold(t)(_ => 0)(_ max _ + 1 )

  // TODO - why is the 'casting' to Tree[B] necessary in the f func? Stuck without the book solution
  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((a) => Leaf(f(a)): Tree[B])((fl, fr) => Branch(fl, fr))
}
val t1 = Branch(Leaf(1), Leaf(2))
val t2 = Branch(Leaf(3), Leaf(4))
val t3 = Branch(t1, t2)

//3.25
Tree.size(t3) // 7

//3.26
Tree.maximum(t3) // 4
//3.27
Tree.depth(t3) // 4

//3.28
Tree.map(t3)(_ + 10)
//3.29
Tree.fold(t3)(_ + 10)(_ + _)
Tree.sizeFold(t3) // 7
Tree.maxFold(t3) // 4
Tree.depthFold(t3) // 2
Tree.mapFold(t3)(_ + 10) // 2
