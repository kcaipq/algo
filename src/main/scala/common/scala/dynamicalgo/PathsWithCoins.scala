package common.scala.dynamicalgo

/**
  * Created by kcai on 28/08/2016.
  * Given a matrix where every cell has some number of coins. Count number of ways to reach bottom right from top left with exactly k coins. We can move to (i+1, j) and (i, j+1) from a cell (i, j).

Example:

Input:  k = 12
        mat[][] = { {1, 2, 3},
                    {4, 6, 5},
                    {3, 2, 1}
                  };
Output:  2
There are two paths with 12 coins
1 -> 2 -> 6 -> 2 -> 1
1 -> 2 -> 3 -> 5 -> 1
  *
  */
object PathsWithCoins extends App {

  def solve(mat: Array[Array[Int]], m: Int, n: Int, k: Int): Int = {
    if (m < 0 || n < 0) 0
    else if (m == 0 && n == 0) if (k == mat(m)(n)) 1 else 0
    else solve(mat, m - 1, n, k - mat(m)(n)) + solve(mat, m, n - 1, k - mat(m)(n))
  }

  val mats = Array.ofDim[Int](3, 3)
  mats(0)(0) = 1
  mats(0)(1) = 2
  mats(0)(2) = 3
  mats(1)(0) = 4
  mats(1)(1) = 6
  mats(1)(2) = 5
  mats(2)(0) = 3
  mats(2)(1) = 2
  mats(2)(2) = 1

  println(solve(mats, 2, 2, 11))
}
