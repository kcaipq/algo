package common.scala.dynamicalgo

import scala.collection.mutable

/**
  * Given a value N, if we want to make change for N cents, and we have infinite supply of each of S = { S1, S2, .. , Sm} valued coins, how many ways can we make the change? The order of coins doesn’t matter.

For example, for N = 4 and S = {1,2,3}, there are four solutions: {1,1,1,1},{1,1,2},{2,2},{1,3}. So output should be 4. For N = 10 and S = {2, 5, 3, 6}, there are five solutions: {2,2,2,2,2}, {2,2,3,3}, {2,2,6}, {2,3,5} and {5,5}. So the output should be 5.

1) Optimal Substructure
To count total number solutions, we can divide all set solutions in two sets.
1) Solutions that do not contain mth coin (or Sm).
2) Solutions that contain at least one Sm.
Let count(S[], m, n) be the function to count the number of solutions, then it can be written as sum of count(S[], m-1, n) and count(S[], m, n-Sm).

Therefore, the problem has optimal substructure property as the problem can be solved using solutions to subproblems.
  */
object CoinAllPossibleChanges extends App {

  def solve(coins: List[Int], size: Int, sum: Int): Int = {

    // base case
    if (sum == 0) 1
    else if (sum < 0) 0
    else if (size <= 0 && sum >= 1) 0
    else // count is sum of solutions (i) including S[m-1] (ii) excluding S[m-1]
      solve(coins, size - 1, sum) + solve(coins, size, sum - coins(size - 1))
  }

  // a fixed amount and a list of coins - find all possible solutions that all coins can make the amount
  // http://www.geeksforgeeks.org/dynamic-programming-set-7-coin-change/
  def solveDP(coins: List[Int], size: Int, amount: Int): Int = {
    val tmp = mutable.Map((1 to amount).map(_ -> 0): _*)
    tmp(0) = 1

    for {
      a <- 0 to size - 1
      c <- coins(a) to amount
    } {
      /*// include
      val x = if ((a - coins(c)) >= 0) tmp(a - coins(c)) else 0
      // exclude
      val y = if (a >= 1) tmp(a) else 0

      tmp(a) = x + y*/

      tmp(c) = tmp(c) + tmp(c - coins(a))
    }

    tmp(amount)
  }

  println(solve(List(1, 2, 3), 3, 10))
  println(solveDP(List(1, 2, 3), 3, 10))
  println(solveDP(List(1, 2, 5, 10), 4, 20))
}
