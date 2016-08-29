package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  *
  * Given weights and values of n items, put these items in a knapsack of capacity W to get the maximum total value in
  * the knapsack. In other words, given two integer arrays val[0..n-1] and wt[0..n-1] which represent values and weights
  * associated with n items respectively. Also given an integer W which represents knapsack capacity, find out the maximum
  * value subset of val[] such that sum of the weights of this subset is smaller than or equal to W. You cannot break an item,
  * either pick the complete item, or don’t pick it (0-1 property).
  *
  */
object KnapSack extends App {

  def knapsack(W: Int, wt: List[Int], va: List[Int], n: Int): Int = {
    if (n == 0 || W == 0) 0
    else if (wt(n - 1) > W) knapsack(W, wt, va, n - 1)
    else Math.max((va(n - 1) + knapsack(W - wt(n - 1), wt, va, n - 1)), knapsack(W, wt, va, n - 1))
  }

  println(knapsack(10, List(7, 5, 3, 3), List(42, 1, 12, 12), 4))

}
