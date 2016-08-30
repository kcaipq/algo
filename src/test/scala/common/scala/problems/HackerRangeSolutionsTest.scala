package common.scala.problems

import common.scala.greedyalgo.MinSumTwoArrays
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by kcai on 26/05/2016.
  */
class HackerRangeSolutionsTest extends FunSuite with Matchers {

  test("Count size of a list without using size() method") {
    HackRangeSolutions.size(List(1, 2, 3, 4, 5)) should be(5)
  }

  test("List replication") {
    val result = HackRangeSolutions.listReplication(3, List(1, 2, 3, 4, 5))
    println(result)
    result.size should be(15)
  }

  test("Test filter with index") {
    val result = HackRangeSolutions.filterByIndex(List(1, 2, 3, 4, 5))
    println(result)
  }

  test("Reverse list") {
    val result = HackRangeSolutions.reverse(List(1, 2, 3, 4, 5))
    println(result)
  }

  test("Print odd number") {
    HackRangeSolutions.printOddNumber(1, 99)
  }

  test("Find last number in a list") {
    val result = HackRangeSolutions.findLastNumber(List(100, 22, 44, 102, 1, 0))
    result should be(0)
  }

  test("Rotate to left") {
    HackRangeSolutions.rotate(-3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    val n = io.Source.stdin.bufferedReader().readLine()
    println(HackRangeSolutions.sum(Array("5", "1", "2", "3", "4", "5")))
  }

  test("Test equilibrium") {
    HackRangeSolutions.equilibrum(Array(-1, 3, -4, 5, 1, -6, 2, 1))
    //HackRangeSolutions.equilibrum(Array(-1, 3, -4, 5, 1, -6, 2, 1))
  }

  test("Test missing minimal positive integer") {
    val result = HackRangeSolutions.missingInteger(Array(1,3,6,4,1,2))
    result should be(5)
  }

  test("Frog cross river") {
    val result = HackRangeSolutions.frogCrossRiver2(5, Array(1, 3, 1, 4, 2, 3, 5, 4))
    result should be(6)
    val r = HackRangeSolutions.frogCrossRiver(5, Array(1, 3, 1, 4, 2, 3, 3, 3, 3, 3, 3, 3, 5, 4))
    r should be (12)
  }

  test("Max profit") {
    val result = HackRangeSolutions.maxProfit(Array(23171, 21011, 21123, 21366, 21013, 21367))
    result should be(356)
  }

  test("Test binary gap"){
    val result = HackRangeSolutions.binaryGap(1041)
    result should be(5)
    //HackRangeSolutions.binaryGap(9) should be (2)
    //HackRangeSolutions.binaryGap(529) should be (4)
    HackRangeSolutions.binaryGap(6) should be(0)
    HackRangeSolutions.binaryGap(328) should be(2)
  }

  test("Cyclic rotation") {
    HackRangeSolutions.cyclicRotation(Array(3, 8, 9 , 7, 6), 3) should be (Array(9, 7, 6, 3, 8))
    HackRangeSolutions.cyclicRotation(Array(3, 8, 9 , 7, 6), 0) should be (Array(3, 8, 9 , 7, 6))
    HackRangeSolutions.cyclicRotation(Array(1, 1, 2, 3, 5), 42) should be (Array(3, 5, 1 , 1, 2))
  }

  test("Odd occurrences") {
    HackRangeSolutions.oddOccurrencesInArray(Array(9, 3, 9, 3, 9, 7, 9)) should be(7)
  }

  test("Dominator") {
    HackRangeSolutions.dominator(Array(3, 4, 3, 2, 3, -1, 3, 3)) should be("0,2,4,6,7")
    HackRangeSolutions.dominator(Array(3, 4, 3, 2, 1, -1, 3, 3)) should be("-1")
    HackRangeSolutions.dominator(Array(3, 4, 3, 2, 1, -1, 3, 3)) should be("-1")
    HackRangeSolutions.dominator(Array()) should be("-1")
  }

  test("Phone bill") {
    HackRangeSolutions.phoneBill("\n   \n   \n   \n   \n   ") should be (0)
    HackRangeSolutions.phoneBill("") should be (0)
    HackRangeSolutions.phoneBill("00:04:01,701-080-080") should be (0)
    HackRangeSolutions.phoneBill("00:01:07,400-234-090\n   00:05:01,701-080-080\n   00:05:00,400-234-090") should be (900)
    HackRangeSolutions.phoneBill("00:01:07,400-234-090\n   00:05:01,701-080-080\n   00:05:00,400-234-090\n   00:06:07,400-234-091") should be (1950)
    HackRangeSolutions.phoneBill("00:01:07,400-234-092\n   00:05:01,701-080-080\n   00:05:00,400-234-090\n   00:06:07,400-234-091") should be (67 * 3 + 11 * 150)
    HackRangeSolutions.phoneBill("00:04:01,701-080-080\n   00:04:01,701-080-081") should be (241 * 3)
    HackRangeSolutions.phoneBill("10:04:01,701-080-080\n   10:04:01,701-080-081") should be ((10 * 60 + 5)  * 150)
    HackRangeSolutions.phoneBill("00:00:01,701-080-080\n   00:00:01,701-080-081") should be (3)
  }

  test("FrogJmp") {
    HackRangeSolutions.frogJmp(1,1,1) should be (0)
    HackRangeSolutions.frogJmp(1,2,2) should be (1)
    HackRangeSolutions.frogJmp(1,2,1) should be (1)
    HackRangeSolutions.frogJmp(10,85,30) should be (3)
    HackRangeSolutions.frogJmp(10,85,30) should be (3)
    HackRangeSolutions.frogJmp(1,85,30) should be (3)
  }

  test("minAbsSumOfTwo2") {
    MinSumTwoArrays.minAbsSumOfTwo2(Array(1, 4, -3)) should be(1)
    HackRangeSolutions.minAbsSumOfTwo2(Array(-8, 4, 5, -10, 3)) should be(3)
    HackRangeSolutions.minAbsSumOfTwo2(Array(-8, 4, 5, -10, 3)) should be(3)
    HackRangeSolutions.minAbsSumOfTwo2(Array(1, 2, 3, 4, 5)) should be(2)
    HackRangeSolutions.minAbsSumOfTwo2(Array(-8)) should be(16)
  }

  test("Fish") {
    var result = HackRangeSolutions.fish(Array(4,3,2,1,5), Array(0,1,0,0,0))
    result should be (2)

    result = HackRangeSolutions.fish(Array(4,3,2,1,5,7,2), Array(0,1,0,0,0,1,0))
    result should be (3)

    result = HackRangeSolutions.fish(Array(1,2,3), Array(0,0,0))
    result should be (3)

    result = HackRangeSolutions.fish(Array(1,2,3), Array(1,1,1))
    result should be (3)

    result = HackRangeSolutions.fish(Array(1,2), Array(1,1))
    result should be (2)
  }

  test("Fizz Buzz simulator") {
    val result = HackRangeSolutions.simulator(List(1,2,3,4,5,6,7,8,9,10))
    println(result)
  }
}