package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  * LCS for input Sequences “ABCDGH” and “AEDFHR” is “ADH” of length 3.
LCS for input Sequences “AGGTAB” and “GXTXAYB” is “GTAB” of length 4.
  */
object LongestCommonSubsequence extends App {

  def solve(a: String, b: String) = {
    def solveLocal(a: String, b: String, ma: Int, mb: Int): Int = {
      if (ma == 0 || mb == 0) 0
      else if (a.charAt(ma - 1) == b.charAt(mb - 1))
        1 + solveLocal(a, b, ma - 1, mb - 1)
      else
        Math.max(solveLocal(a, b, ma - 1, mb), solveLocal(a, b, ma, mb - 1))
    }

    solveLocal(a, b, a.length, b.length)
  }

  println(solve("AGGTAB", "AABGGT"))
  println(solve("BDAECFG", "ABCDEFG"))


  // =================================================================================
  def solveDP(a: String, b: String, ma: Int, mb: Int) = {
      val tmp = Array.ofDim[Int](ma + 1, mb + 1)

      for {
        i <- (0 to ma - 1).reverse
        j <- (0 to mb - 1).reverse
      } {
        if (i == 0 || j == 0) 0
        else if (a.charAt(i - 1) == b.charAt(j - 1)) tmp(i)(j) = tmp(i - 1)(j - 1) + 1
        else tmp(i)(j) = Math.max(tmp(i - 1)(j), tmp(i)(j - 1))
      }

      tmp(ma)(mb)
  }

  println(solve("AGGTAB", "GXTXAYB"))



  // ==========================

  def solveLCSListDP(a: List[String], b: List[String], ma: Int, mb: Int) = {
    val tmp = Array.ofDim[Int](ma + 1, mb + 1)

    for {
      i <- (0 to ma - 1).reverse
      j <- (0 to mb - 1).reverse
    } {
      if (i == 0 || j == 0) 0
      else if (a(i - 1) == b(j - 1)) tmp(i)(j) = tmp(i - 1)(j - 1) + 1
      else tmp(i)(j) = Math.max(tmp(i - 1)(j), tmp(i)(j - 1))
    }

    tmp(ma)(mb)
  }

  // ====================
  def solveLCSREC(a: List[String], b: List[String]) = {
    def solveLocal(a: List[String], b: List[String], ma: Int, mb: Int): Int = {
      if (ma == 0 || mb == 0) 0
      else if (a(ma - 1) == b(mb - 1))
        1 + solveLocal(a, b, ma - 1, mb - 1)
      else
        Math.max(solveLocal(a, b, ma - 1, mb), solveLocal(a, b, ma, mb - 1))
    }

    solveLocal(a, b, a.length, b.length)
  }

  val la = List("10", "22", "9", "33", "21", "50", "41", "60")
  val lb = List("9", "10", "21", "22", "33", "41", "50", "60")
  println(solveLCSListDP(la, lb, la.size, lb.size))
}
