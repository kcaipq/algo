package common.scala.greedyalgo

/**
  * Created by kcai on 28/08/2016.
  */
object ShortestCommonSuperStr extends App {

  def merge(s1: String, s2: String, len: Int) = {
    s1 + s2.substring(len)
  }

  def overlap(s1: String, s2: String): Int = {
    val ss = for {
      r1 <- (0 to (s1.length - 1)).reverse
      r2 <- (1 to s2.length)
      if (s1.substring(r1) == s2.substring(0, r2))
    } yield r2

    if (ss.isEmpty) 0 else ss.head
  }

  def findOverlap(list: List[String]) = {
    var max = 0
    var overS1 = ""
    var overS2 = ""

    for {
      l1 <- list
      l2 <- list
      if (l1 != l2)
    } yield {
      val currentOverlap = overlap(l1, l2)
      if (currentOverlap > max) {
        max = currentOverlap
        overS1 = l1
        overS2 = l2
      }
    }
    val added = merge(overS1, overS2, max) :: list
    added.filter(a => a != overS1 && a != overS2)
  }

  def solve(input: List[String]) = {
    def solveLocal(acc: List[String]): String = {
      acc match {
        case List(a) => a
        case list => solveLocal(findOverlap(list))
      }
    }
    solveLocal(input)
  }

  println(solve(List("catg", "ctaagt", "gcta", "ttca", "atgcatc")))

}
