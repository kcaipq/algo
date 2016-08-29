package common.scala.greedyalgo

/**
  * Created by kcai on 29/08/2016.
  */
object GreedyCoins extends App {



  // ===========================================================================
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


  // =======================================================================
  def divMod(dividend: Int, divisor: Int) = {
    (dividend / divisor, dividend % divisor)
  }

  // the list has to be sorted in descending order of denomination
  def change(coins: List[Int], amount: Int) = {
    def changeOne(pair: (List[Int], Int), div: Int) = {
      val dm = divMod(pair._2, div)
      ((dm._1 :: pair._1), dm._2)
    }

    ((List[Int](), amount) /: coins) (changeOne(_, _))._1.reverse
  }

  println(change(List(25, 10, 5, 1), 71))


  // ===========================================================================
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
}
