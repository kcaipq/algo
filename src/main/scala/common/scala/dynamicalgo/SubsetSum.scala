package common.scala.dynamicalgo

/**
  * http://www.geeksforgeeks.org/dynamic-programming-subset-sum-problem/
  *
  * Created by kcai on 29/08/2016.
  * Given a set of non-negative integers, and a value sum, determine if there is a subset of the given set with sum equal to given sum.
  * *
  * Examples: set[] = {3, 34, 4, 12, 5, 2}, sum = 9
  * Output:  True  //There is a subset (4, 5) with sum 9.
  * *
  *
  * The isSubsetSum problem can be divided into two subproblems
  * …a) Include the last element, recur for n = n-1, sum = sum – set[n-1]
  * …b) Exclude the last element, recur for n = n-1.
  *
  *
  */
object SubsetSum extends App {

  def solve(s: List[Int], n: Int, sum: Int): Boolean = {

    if (sum == 0) true

    else if (n == 0 && sum != 0) false
      // ignore the one is greater than the sum
    else if (s(n - 1) > sum) solve(s, n - 1, sum)

    else solve(s, n - 1, sum) || solve(s, n - 1, sum - s(n - 1))

  }

  println(solve(List(3, 34, 4, 12, 5, 2), 6, 9))

}
