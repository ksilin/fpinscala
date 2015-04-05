object session {

  def curry[A,B,C](f:(A,B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // overwriting addition
  def add(a: Int, b: Int): Int = a + b

  val curriedAdd: Int => Int => Int = curry(add)
  val add3 = curriedAdd(3)
  add3(5) // 8

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a, b) => f(a)(b)
  }

  private val readd: (Int, Int) => Int = uncurry(curriedAdd)
  readd(5, 7) //12

}