package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  */
object CoinMinimumChanges extends App {

  implicit val coins = List(1, 2, 5, 10, 20, 50, 100, 200)
  import collection.mutable.Map

  def countChange(amount: Int)(implicit coins: List[Int]) = {
    val min = Map((1 to amount).map (_ -> (Int.MaxValue, "")): _*)
    min(0) = (0, "")

    (1 to amount) flatMap { currentAmount =>
      coins.map { coin =>
        if (coin <= currentAmount && min(currentAmount - coin)._1 + 1 < min(currentAmount)._1) {
          min(currentAmount) = (min(currentAmount - coin)._1 + 1, min(currentAmount - coin)._2.toString + "," + coin)
        }
      }
    }
    min(amount)
  }

  println(countChange(97))
  println(countChange(200))

}
