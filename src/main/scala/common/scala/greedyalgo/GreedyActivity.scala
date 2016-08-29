package common.scala.greedyalgo

/**
  * Created by kcai on 29/08/2016.
  */
object GreedyActivity extends App {

  def greedyActivity(start: List[Int], finish: List[Int]) = {
    val startFinish = start zip finish sortBy(_._2)

    def select(target: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
      target match {
        case Nil => acc
        case head :: second :: tail =>
          // head is the selected one
          if (second._1 >= head._2) select(second :: tail, head :: acc)
          else select(tail, head :: acc)

        case head :: Nil =>
          if (acc.head._2 <= head._1)
            select (Nil, head :: acc)
          else acc

      }
    }

    select(startFinish, Nil)
  }

  val result =  greedyActivity(List(1,3,0,5,8,5), List(2,4,6,7,9,9))
  println(result)

}
