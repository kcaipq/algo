package common.scala.problems

/**
  * Created by kcai on 29/08/2016.
  */
object MinSumTwoArrays extends App {

  def minAbsSumOfTwo2(A: Array[Int]): Int = {
    val sorted = A.sorted
    val size = A.length

    var left = 0
    var right = size - 1
    var minAbs = Math.abs(sorted(left) + sorted(right))

    while(left <= right) {
      val currentSum = sorted(left) + sorted(right)
      minAbs = Math.min(minAbs, Math.abs(currentSum))

      if (currentSum <= 0)
        left = left + 1
      else right = right - 1
    }

    minAbs
  }
}
