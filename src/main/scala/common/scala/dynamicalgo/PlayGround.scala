package common.scala.dynamicalgo

import scala.collection.mutable


object PlayGround extends App {

  //=====================================================================================================
  def minSubsetPartition(n: List[Int], m: Int, total: Int): Int = {
    def solveLocal(input: List[Int], size: Int, accSum: Int): Int = {

      if (size == 0) Math.abs((total - accSum) - accSum)
      else {
        Math.min(solveLocal(input, size - 1, accSum + input(m - 1)), solveLocal(input, size - 1, accSum))
      }

    }
    solveLocal(n, n.size, 0)
  }

  //=====================================================================================================
  def subsetSum(n: List[Int], m: Int, sum: Int): Boolean = {
    if (sum == 0) true
    else if (m == 0 && sum != 0) false
    else if (n(m - 1) > sum) subsetSum(n, m - 1, sum)
    else {
      subsetSum(n, m - 1, sum - n(m - 1)) || subsetSum(n, m - 1, sum)
    }
  }

  //=====================================================================================================
  def minCoinChangesDP(n: List[Int], amount: Int) = {
    // build the container to store the state
    val dp = mutable.Map((1 to amount).map(_ -> Int.MaxValue): _*)
    // base case
    dp(0) = 0

    // loop through all the coins and find combination
    for {
      a <- 1 to amount
      c <- n
    } {
      if (c <= a && (dp(a - c) + 1) < dp(a))
        dp(a) = dp(a - c) + 1
    }

    dp(amount)
  }

  //=====================================================================================================
  def minCoinChanges(n: List[Int], amount: Int): Int = {

    def localM(n: List[Int], count: Int, amount: Int): Int = {

      amount match {
        // no match return the current count
        case x if (x < 0) => count
        case x if (n.isEmpty && x == 0) => count + 1
        case x if (n.isEmpty && x != 0) => count
        //case x if (x > 0) => count
        case x =>
          // two possibilities
          // inclusive and exclusive
          localM(n.tail, count, amount) + localM(n, count, amount - n.head)
      }
    }
    localM(n, 0, amount)
  }

  //=====================================================================================================
  def countChange(money: Int, coins: List[Int]): Int = {
    def change(m: Int, coinList: List[Int], count: Int): Int =
      m match {
        case _ if m < 0 => count
        case _ if coinList.isEmpty => {
          m match {
            case 0 => count + 1
            case _ => count
          }
        }

        // exclusive and inclusive
        case _ => change(m, coinList.tail, count) + change(m - coinList.head, coinList, count)
      }
    change(money, coins, 0)
  }

  //===================================================================================================
  def moneyChanges(money: Int, coins: List[Int]) : Option[List[Seq[Int]]]= {
    var listOfChange = List[Seq[Int]]()
    def changeMoney(amount: Int, coins: List[Int], result: Option[Seq[Int]]): Int = {
      if (amount == 0) {
        listOfChange = result.get :: listOfChange
        1
      } else if (amount < 0)
        0
      else if (coins.isEmpty && amount >= 1)
        0
      else {
        // exclusive and inclusive
        changeMoney(amount, coins.tail, result) + changeMoney(amount - coins.head, coins, Some(coins.head +: result.getOrElse(Seq())))
      }
    }

    changeMoney(money, coins.sortWith(_.compareTo(_) < 0), None)
    Some(listOfChange)
  }

  println(minCoinChangesDP(List(1, 2, 5, 10), 20))
  println(minCoinChanges(List(1, 2, 5, 10), 20))
  println(countChange(20, List(1, 2, 5, 10)))

}
