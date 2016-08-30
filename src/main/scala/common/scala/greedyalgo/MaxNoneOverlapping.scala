package common.scala.greedyalgo

/**
  * Created by ahenry on 30/08/16.
  */
object MaxNoneOverlapping extends App {

  def solve(a: List[Int], b: List[Int]) = {
    val n = a.size

    if (n <= 1) n
    else {
      var result = 1
      var end = b(0)


      for(cuur <- 1 to n - 1) {
        if (a(cuur) > end) {
          result = result + 1
          end = b(cuur)
        }
      }
      result
    }
  }

  println(solve(List(1, 3, 7, 9, 9), List(5, 6, 8, 9, 10)))

}
