package common.scala.problems

/**
  * The equilibrium index of a sequence is an index such that the sum of elements at lower indexes is equal to the sum of elements at higher indexes. For example, in a sequence A:
  * A[0]=-7 A[1]=1 A[2]=5 A[3]=2 A[4]=-4 A[5]=3 A[6]=0
  * 3 is an equilibrium index, because:
  * A[0]+A[1]+A[2]=A[4]+A[5]+A[6]
  * 6 is also an equilibrium index, because:
  * A[0]+A[1]+A[2]+A[3]+A[4]+A[5]=0
  */
object Equilibrum extends App {
  def equilibrum(a: Array[Int]): Int = {
    // Partial sums are Longs to avoid Int overflow.
    val sums = a.scanLeft(0L)(_ + _).tail
    def equilibrium(index: Int) = left(index) == right(index)
    def left(index: Int) = if (index == 0) 0L else sums(index - 1)
    def right(index: Int) = sums.last - sums(index)
    (0 until a.length) find (equilibrium(_)) getOrElse (-1)
  }
}
