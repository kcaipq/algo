package common.scala.greedyalgo


object NumberSolitaire extends App {

  def solve(a: List[Int]) = {
    val dp = Array.fill(a.size)(a(0))
    val lens = a.size

    for (i <- 1 to lens - 1) {
      dp(i % 6) = dp.max + a(i)
    }

    dp((lens - 1) % 6)

  }

  println(solve(List(1, -2, 0, 9, -1, -2)))

}
