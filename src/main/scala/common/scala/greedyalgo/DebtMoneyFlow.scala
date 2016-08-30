package common.scala.greedyalgo

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


/**
  * Do following for every person Pi where i is from 0 to n-1.
1) Compute the net amount for every person. The net amount for person ‘i’ can be computed be subtracting sum of all debts from sum of all credits.

2) Find the two persons that are maximum creditor and maximum debtor. Let the maximum amount to be credited maximum creditor be maxCredit and maximum amount to be debited from maximum debtor be maxDebit. Let the maximum debtor be Pd and maximum creditor be Pc.

3) Find the minimum of maxDebit and maxCredit. Let minimum of two be x. Debit ‘x’ from Pd and credit this amount to Pc

4) If x is equal to maxCredit, then remove Pc from set of persons and recur for remaining (n-1) persons.

5) If x is equal to maxDebit, then remove Pd from set of persons and recur for remaining (n-1) persons.
  */
object DebtMoneyFlow extends App {

  def minCash(amount: ListBuffer[Int]): Unit = {
    val maxCredit = amount.zipWithIndex.maxBy(_._1)._2
    val maxDebt = amount.zipWithIndex.minBy(_._1)._2

    if (amount(maxCredit) == 0 && amount(maxDebt) == 0) ()
    else {
      val min = Math.min(-amount(maxDebt), amount(maxCredit))

      amount(maxCredit) -= min
      amount(maxDebt) += min

      println("Person " + maxDebt + " pays " + min + " to person " + maxCredit)

      if (min != 0)
        minCash(amount)
    }
  }

  def buildAmount(g: Array[Array[Int]], m: Int) = {
    val amount = ListBuffer.fill(m)(0)

    for {
      p <- 0 to m - 1
      i <- 0 to m - 1
    } {
      amount(p) += g(i)(p) - g(p)(i)
    }

    minCash(amount)
  }

  val g = Array.ofDim[Int](3, 3)
  g(0)(0) = 0
  g(0)(1) = 1000
  g(0)(2) = 2000
  g(1)(0) = 0
  g(1)(1) = 0
  g(1)(2) = 5000
  g(2)(0) = 0
  g(2)(1) = 0
  g(2)(2) = 0

  buildAmount(g, 3)

}
