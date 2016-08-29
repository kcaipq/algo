package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  * Given a n*n matrix where numbers all numbers are distinct and are distributed from range 1 to n2, find the maximum length path (starting from any cell) such that all cells along the path are increasing order with a difference of 1.
  *
  */
object LongestMatrixPath extends App {

  def solve(n: Int, mat: Array[Array[Int]]) = {
    var result = 0

    // 2d array table to store values
    val dp = Array.fill(n)(Array.fill(n)(-1))

    for {
      i <- 0 to n - 1
      j <- 0 to n - 1
    } {
      if (dp(i)(j) == -1)
        // find the longest from cell
        buildDpTable(i, j, mat, dp, n)

      result = Math.max(result, dp(i)(j))
    }

    result
  }

  def buildDpTable(i: Int, j: Int, mat: Array[Array[Int]], dp: Array[Array[Int]], n: Int): Int = {
    if (i < 0 || i >= n || j < 0 || j >= n) 0
    else if (dp(i)(j) != -1) dp(i)(j)
    else {
      if (j < n - 1 && (mat(i)(j) + 1) == mat(i)(j + 1))
        dp(i)(j) = 1 + buildDpTable(i, j + 1, mat, dp, n)
      else if (j > 0 && (mat(i)(j) + 1 == mat(i)(j - 1)))
        dp(i)(j) = 1 + buildDpTable(i ,j - 1, mat, dp, n);
      else if (i > 0 && (mat(i)(j) + 1 == mat(i - 1)(j)))
        dp(i)(j) = 1 + buildDpTable(i - 1, j, mat, dp, n);
      else if (i < n - 1 && (mat(i)(j) + 1 == mat(i + 1)(j)))
        dp(i)(j) = 1 + buildDpTable(i + 1, j, mat, dp, n);
      else dp(i)(j) = 1

      dp(i)(j)
    }
  }

  val input = Array.ofDim[Int](3, 3)
  input(0)(0) = 1
  input(0)(1) = 2
  input(0)(2) = 9
  input(1)(0) = 5
  input(1)(1) = 3
  input(1)(2) = 8
  input(2)(0) = 4
  input(2)(1) = 6
  input(2)(2) = 7


  println(solve(input.length, input))

}
