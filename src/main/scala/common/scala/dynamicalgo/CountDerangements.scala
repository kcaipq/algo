package common.scala.dynamicalgo

import collection.mutable.Map

/**
  * A Derangement is a permutation of n elements, such that no element appears in its original position. For example, a derangement of {0, 1, 2, 3} is {2, 3, 1, 0}.

Given a number n, find total number of Derangements of a set of n elements.

Examples:

Input: n = 2
Output: 1
For two elements say {0, 1}, there is only one
possible derangement {1, 0}

Input: n = 3
Output: 2
For three elements say {0, 1, 2}, there are two
possible derangements {2, 0, 1} and {1, 2, 0}

Input: n = 4
Output: 9
For four elements say {0, 1, 2, 3}, there are 9
possible derangements {1, 0, 3, 2} {1, 2, 3, 0}
{1, 3, 0, 2}, {2, 3, 0, 1}, {2, 0, 3, 1}, {2, 3,
1, 0}, {3, 0, 1, 2}, {3, 2, 0, 1} and {3, 2, 1, 0}
  */
object CountDerangements extends App {

  def solve(n: Int) = {
    val i = Map((3 to n).map(_ -> 0): _*)
    i(0) = 0
    i(1) = 0
    i(2) = 1

    for(r <- 3 to n) {
      i(r) = (r - 1) * (i(r - 1) + i(r - 2))
    }

    i(n)
  }


  val s = solve(5)
  println(s)

}
