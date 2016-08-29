package common.scala.dynamicalgo

import scala.collection.mutable

/**
  * Created by kcai on 29/08/2016.
  */
object Fibonacci extends App {

  def fib(n: Int): Int = {
    if (n == 0) n
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  // =====================================================
  def fibDP(n: Int): Int = {
    // map for storing subproblems
    val tmp = mutable.Map((0 to n).map(_ -> 0): _*)

    if (tmp(n) == 0) {
      if (n == 0)
        tmp(n) = n
      else if (n == 1)
        tmp(n) = 1
      else
        tmp(n) = fib(n - 1) + fib(n - 2)
    }

    tmp(n)
  }

  println(fibDP(6))
  println(fib(6))

}
