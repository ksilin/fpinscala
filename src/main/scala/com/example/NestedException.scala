package com.example

import scala.util.control.NonFatal

// Daniel experienced the exception thrown by c being caught by a,
// although in a more complex real-world situation
object NestedException extends App {

  var retries = 10

  def a(): Unit = {
    try {
      b()
    } catch {
      case NonFatal(e) => println(s"a caught ${e}")
    }
  }

  def b(): Unit = {
    try {
      c()
    } catch {
      case NonFatal(e) => println(s"b caught ${e}, retries: ${retries}")
        if (retries > 0) {
          retries -= 1
          b()
        }
        else throw e
    }
  }

  def c(): Unit = throw new Exception("c threw")

  (1 to 10).map(_ => a())
}
