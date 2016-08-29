package common.scala.problems

import scala.annotation.tailrec

/**
  * Created by kcai on 26/06/2016.
  */
object LeftTruncate extends App {

  def leftTruncate(num: Int): List[Int] = {
    @tailrec
    def helper(n: Int, acc: Seq[Int]): Seq[Int] = {
      n match {
        case x if x <= 9 => acc :+ x
        case x if x > 9 =>
          val m = Math.pow(10, x.toString.length - 1).toInt
          helper(x % m, acc :+ x)
      }
    }

    //helper(num, Nil).toList
    var l = num.toString
    var s = Seq[String]()
    while(l.length > 0) {
      s = s :+ l
      l = l.drop(1)
    }
    s.toList.map(_.toInt)
  }

  println(leftTruncate(1234))
  println(leftTruncate(0))
  println(leftTruncate(10))
  println(leftTruncate(99))

}
