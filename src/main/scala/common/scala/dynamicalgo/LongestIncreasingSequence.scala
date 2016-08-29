package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  */
object LongestIncreasingSequence extends App {


  def solveLCS(a: List[String], b: List[String]) = {
    def solveLocal(a: List[String], b: List[String], ma: Int, mb: Int): Int = {
      if (ma == 0 || mb == 0) 0
      else if (a(ma - 1) == b(mb - 1))
        1 + solveLocal(a, b, ma - 1, mb - 1)
      else
        Math.max(solveLocal(a, b, ma - 1, mb), solveLocal(a, b, ma, mb - 1))
    }

    solveLocal(a, b, a.length, b.length)
  }

  // =============================================================================
  def solveLIS(n: List[Int]) = {
    val copy = n.sorted
    val removed = copy.distinct
    val s1 = n.map(_.toString)
    val s2 = removed.map(_.toString)


    solveLCS(s1, s2)
  }


  println(solveLIS(List(10, 22, 9, 33, 21, 50, 41, 60)))


  // =============================================================================
  def longestIncrSubseq[T](xs: List[T])(implicit ord: Ordering[T]) = {
    xs.foldLeft(List[(Int, List[T])]()) {
      (sofar, x) =>
        if (sofar.isEmpty) List((1, List(x)))
        else {
          val resIfEndsAtCurr = (sofar, xs).zipped map {
            (tp, y) =>
              val len = tp._1
              val seq = tp._2
              if (ord.lteq(y, x)) {
                (len + 1, x :: seq) // reversely recorded to avoid O(n)
              } else {
                (1, List(x))
              }
          }
          sofar :+ resIfEndsAtCurr.maxBy(_._1)
        }
    }.maxBy(_._1)._2.reverse
  }

  println(longestIncrSubseq(List(
    0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)))
}
