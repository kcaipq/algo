package common.scala.problems

/**
  * Created by kcai on 29/08/2016.
  */
object IncreaseSequence extends App {

  def increaseInt(n: Seq[Int]): Seq[Int] = {
    def increase(s: Seq[Int]): Seq[Int] =
      s match {
        case Nil => Nil
        case 9 +: Nil => Seq(0, 1)
        case 9 +: xs => 0 +: increase(xs)
        case x +: xs => (x + 1) +: xs
      }
    increase(n.reverse).reverse
  }

  // another way of doing it - map to int and increase by 1 and map back to Seq
  def increaseInt2(n: Seq[Int]): Seq[Int] = {
    n match {
      case Nil => Nil
      case _ =>
        def toSeq(n: Int): Seq[Int] = {
          n match {
            case 0 => Nil
            case _ =>  toSeq(n / 10) :+ (n % 10)
          }
        }
        val rawInt = n.foldLeft((n.length, 0)) ((acc, x) => (acc._1 - 1, acc._2 + x * scala.math.pow(10, acc._1 - 1).toInt))._2
        if (rawInt == -1) Seq(0)
        else toSeq(rawInt + 1)
    }
  }

}
