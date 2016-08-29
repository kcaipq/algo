package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  */
object PartitionMinimumSubset extends App {

  /**
    * The recursive approach is to generate all possible sums from all the values of array and to check which solution is the most optimal one.
To generate sums we either include the i’th item in set 1 or don’t include, i.e., include in set 2.
    * @param n
    * @param sumTotal
    * @return
    */
  def solve(n: List[Int], sumTotal: Int) = {

    def solveLocal(m: List[Int], size: Int, accSum: Int): Int = {
      if (size == 0)
        Math.abs((sumTotal - accSum) - accSum)
        // inclusive and exclusive
      else Math.min(solveLocal(m, size - 1, accSum + m(size - 1)), solveLocal(m, size - 1, accSum))
    }
    solveLocal(n, n.size, 0)
  }

  val input = List(3, 1, 4, 2, 2, 1)
  println(solve(input, input.sum))

}
