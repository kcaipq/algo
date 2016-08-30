package common.scala.dynamicalgo

/**
  * http://www.geeksforgeeks.org/dynamic-programming-set-8-matrix-chain-multiplication/
  */
object MatrixChainMulplication extends App {

  def solve(p: List[Int], i: Int, j: Int): Int = {
    if (i == j) 0

    var min = Int.MaxValue

    for{
      k <- i to j - 1
    } {
      val a = solve(p, i, k)
      val b = solve(p, k + 1, j)
      val c = p(i - 1)*p(k)*p(j)
      val count = a + b + c

      if (count < min) min = count
    }

    min
  }

  println(solve(List(1, 2, 3, 4, 3), 1, 4))
}
