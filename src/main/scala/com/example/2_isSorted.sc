object session {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.sliding(2).forall((p) => ordered(p(0),p(1)))
  }

  val a = Array(1, 2, 3)
  val b = Array(2, 2, 1)
  isSorted(a, (a: Int, b: Int) =>(a <= b)) // true
  isSorted(b, (a: Int, b: Int) =>(a <= b)) // false
}
