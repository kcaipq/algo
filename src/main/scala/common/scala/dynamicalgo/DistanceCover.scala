package common.scala.dynamicalgo

/**
  * Created by kcai on 29/08/2016.
  *
  * Given a distance ‘dist, count total number of ways to cover the distance with 1, 2 and 3 steps.

Examples:

Input:  n = 3
Output: 4
Below are the four ways
 1 step + 1 step + 1 step
 1 step + 2 step
 2 step + 1 step
 3 step

Input:  n = 4
Output: 7
  *
  */
object DistanceCover extends App {

  def solve(dist: Int): Int = {
    if (dist < 0) 0
    else if (dist == 0) 1
    else solve(dist - 1) + solve(dist - 2) + solve(dist - 3)
  }

  println(solve(4))
}
