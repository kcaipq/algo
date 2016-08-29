package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  *
  * Given two strings str1 and str2 and below operations that can performed on str1. Find minimum number of edits (operations) required to convert ‘str1′ into ‘str2′.
  * *
  * Insert
  * Remove
  * Replace
  * All of the above operations are of equal cost.
  * *
  *
  *
  * The idea is process all characters one by one staring from either from left or right sides of both strings.
  * Let we traverse from right corner, there are two possibilities for every pair of character being traversed.
  * *
  * m: Length of str1 (first string)
  * n: Length of str2 (second string)
  * If last characters of two strings are same, nothing much to do. Ignore last characters and get count for remaining strings. So we recur for lengths m-1 and n-1.
  * Else (If last characters are not same), we consider all operations on ‘str1′, consider all three operations on last character of first string, recursively compute minimum cost for all three operations and take minimum of three values.
  * Insert: Recur for m and n-1
  * Remove: Recur for m-1 and n
  * Replace: Recur for m-1 and n-1
  *
  *
  *
  */
object EditDistance extends App {
  def solve(str1: String, str2: String, m: Int, n: Int): Int = {
    if (m == 0) n
    else if (n == 0) m

    else if (str1.charAt(m - 1) == str2.charAt(n - 1))
      solve(str1, str2, m - 1, n - 1)

    else 1 + min(solve(str1, str2, m, n - 1), solve(str1, str2, m - 1, n), solve(str1, str2, m - 1, n - 1))

  }

  def min(x: Int, y: Int, z: Int) = {
    if (x < y && x < z) x
    else if (y < x && y < z) y
    else z
  }

  val s1 = "sunday"
  val s2 = "saturday"

  println(solve(s1, s2, s1.length, s2.length))
}
