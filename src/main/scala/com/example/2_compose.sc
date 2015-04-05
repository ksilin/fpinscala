object session {

  def compose[A, B, C](f: B => C, g: A => B): A => C= {
    a => f(g(a))
  }

  def numbers(s: String): List[Int] = {
    // need the sliding to receive strings
    // a simple list conversion would return chars
    s.sliding(1).toList map (_.toInt)
  }

  def sum(i: List[Int]): Int = {
    i.reduce(_+_)
  }

  val numSum = compose(sum, numbers)
  numSum("123") // 6

}