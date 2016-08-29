package common.scala.greedyalgo

/**
  * Given n items of different weights and bins each of capacity c, assign each item to a bin such that number of total used bins is minimized. It may be assumed that all items have weights smaller than bin capacity.

Example:

Input:  wieght[]       = {4, 8, 1, 4, 2, 1}
        Bin Capacity c = 10
Output: 2
We need minimum 2 bins to accommodate all items
First bin contains {4, 4, 2} and second bin {8, 2}

Input:  wieght[]       = {9, 8, 2, 2, 5, 4}
        Bin Capacity c = 10
Output: 4
We need minimum 4 bins to accommodate all items.

Input:  wieght[]       = {2, 5, 4, 7, 1, 3, 8};
        Bin Capacity c = 10
Output: 3
  */
object BinPacking extends App {

  private val cap = 10

  def solve(items: Seq[Int], bins: Map[Int, Seq[Int]]): Map[Int, Seq[Int]] = {
    val sortedItems = items.sortWith(_ > _)
    sortedItems match {
      case Nil => bins
      case head :: tail =>

        // find first bin that can fit this item
        bins.find(cap - _._2.sum >= head) match {
          case Some(bin) =>
            val currentCap = bin._2.sum - head
            solve(tail, bins + (bin._1 -> (bin._2 :+ head)))

          case None =>
            // new bin
            val currentCap = cap - head
            solve(tail, bins + (bins.size + 1 -> Seq(head)))
        }
    }
  }

  println(solve(Seq(4, 8, 1, 4, 2, 1), Map.empty))
  println(solve(Seq(2, 5, 4, 7, 1, 3, 8), Map.empty))
  println(solve(Seq(9, 8, 2, 2, 5, 4), Map.empty))
  println(solve(Seq(2, 5, 4, 7, 1, 3, 8), Map.empty))
  println(solve(Seq(8, 5, 7, 7, 9, 7, 8), Map.empty))

}
