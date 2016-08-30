package common.scala.greedyalgo

/**
  * int length = 0;
        for (int rope : A) {
            length += rope;
            if (length >= K) {
                result++;
                length = 0;
            }
        }
        return result;
  *
  *
  */
object TieRope extends App {

  /**
    * greater or equal to K is maximal
    * @param k
    * @param a
    * @return
    */
  def solve(k: Int, a: List[Int]) = {
    val n = a.size

    var result = 0
    var length = 0

    a.foreach { r =>

      length = length + r

      if (length >= k) {
        result = result + 1
        length = 0
      }

    }

    result
  }

  println(solve(4, List(1,2,3,4,1,1,3)))

}
