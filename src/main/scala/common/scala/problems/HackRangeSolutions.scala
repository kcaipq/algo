package common.scala.problems

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by kcai on 26/05/2016.
  */
object HackRangeSolutions {

  // Hacker Range: http://giocc.com/exploring-functional-programming-in-scala-through-hackerrank.html
  // http://sujitpal.blogspot.co.uk/2009/01/scala-mock-interview-coding-questions.html

  def size(arr: List[Int]): Int =
    (0 /: arr)((acc, _) => acc + 1)
    //foldLeft(0)((acc, _) => acc + 1)

  def listReplication(num: Int, arr: List[Int]): List[Int] = arr.foldLeft[List[Int]](List())((acc, x) => acc ++ List.fill(num)(x))

  def listRepetition(num: Int) = List.fill(num)(1)

  def filterByIndex(arr: List[Int]) = arr.view.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).force.toList

  def reverse(arr: List[Int]): List[Int] = arr.foldLeft[List[Int]](List())((acc, x) => x :: acc)

  def reverseString(s: String): String = {
    val len = s.length
    if (len == 1) s
    else {
      reverseString(s.substring(1, len) + s.charAt(0))
    }
  }

  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else {
      fibonacci(n - 2) + fibonacci(n - 1)
    }
  }

  def wordFrequency(file: String) = {
    val lines = Source.fromFile(file).getLines()
    val words = for (line <- lines; word <- line.split("[ ,;.!?]+") if word.trim.length > 0) yield (word)
    words.foldLeft(Map.empty[String, Int])((map: Map[String, Int], w: String) => map + (w -> (1 + map.getOrElse(w, 0))))
  }

  def printOddNumber(min: Int, max: Int) = {
    for (m <- min until max if m % 2 == 1) print(m)
  }

  def findLastNumber(input: List[Int]): Int = {
    input match {
      case h :: Nil => h
      case _ :: tail => findLastNumber(tail)
    }
  }

  def flatten[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case (h:List[A])::tail => flatten(h):::flatten(tail)
    case (h:A)::tail => h::flatten(tail)
  }

  // 99 Scala problems: http://blog.thedigitalcatonline.com/blog/2015/04/07/99-scala-problems-index/#.V0bXTWQrIsn
  // http://aperiodic.net/phil/scala/s-99/

  //P19 (**) Rotate a list N places to the left.
  def rotate[A](n: Int, l: List[A]):List[A] = {
    val wrapn = if (l.isEmpty) 0 else n % l.length
    println(wrapn)
    if (wrapn < 0) rotate(l.length + n, l)
    else l.drop(wrapn):::l.take(wrapn)
  }

  //P09 (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
  def pack[A](l: List[A]):List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]):List[List[A]] = rem match {
      case Nil => res
      case h::tail if (res.isEmpty || res.last.head != h) => _pack(res:::List(List(h)), tail)
      case h::tail => _pack(res.init:::List(res.last:::List(h)), tail)
    }
    _pack(List(),l)
  }

  def pack2[A](l: List[A]):List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]):List[List[A]] = rem match {
      case Nil => res
      case ls => {
        val (s: List[A], r: List[A]) = rem span { _ == rem.head }
        _pack(res:::List(s), r)
      }
    }
    _pack(List(), l)
  }

  //P08 Eliminate consecutive duplicates of list elements. If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  def compress[A](l: List[A]):List[A] = l match {
    case Nil => Nil
    case h::List() => List(h)
    case h::tail if (h == tail.head) => compress(tail)
    case h::tail => h::compress(tail)
  }

  def compress2[A](l: List[A]):List[A] = l.foldLeft(List[A]()) {
    case (ls, e) if (ls.isEmpty || ls.last != e) => ls:::List(e)
    case (ls, e) => ls
  }

  //P16 (**) Drop every Nth element from a list.
  def drop[A](n: Int, l: List[A]):List[A] = {
    l.grouped(n).flatMap { _.take(n - 1) }.toList
  }

  def sum(args: Array[String]) = {
    args.drop(1).foldLeft(0)((acc, x) => acc + x.toInt)
  }

  def equilibrum(a: Array[Int]): Int = {
    // Partial sums are Longs to avoid Int overflow.
    val sums = a.scanLeft(0L)(_ + _).tail
    def equilibrium(index: Int) = left(index) == right(index)
    def left(index: Int) = if (index == 0) 0L else sums(index - 1)
    def right(index: Int) = sums.last - sums(index)
    (0 until a.length) find (equilibrium(_)) getOrElse(-1)
  }

  def missingInteger(a: Array[Int]) = {
    def findMissing(in: Int, l: List[Int]): Int = {
      if (l.isEmpty || l.head != in) in
      else findMissing(in + 1, l.tail)
    }
    findMissing(1, a.toList.filter(_ > 0).distinct.sorted)
  }

  def frogCrossRiver(x: Int, a: Array[Int]): Int = {
    val l: ListBuffer[Int] = List.range(1, x + 1).to[ListBuffer]
    val r = for (s <- a if (l.nonEmpty && l.contains(s))) yield { l -= s
      if (l.isEmpty) a.indexOf(s)
      else -1
    }
    val p = r.toList.filter(_ != -1)
    if (p.isEmpty) -1 else p.head
  }

  def frogCrossRiver2(x: Int, a: Array[Int]) = {
    val l: ListBuffer[Int] = ListBuffer.range(1, x + 1)

    def visited(z: List[(Int, Int)]): Int = {
      z match {
        case Nil => -1
        case h :: tail if (l.isEmpty) => h._2
        case h :: tail if (l.nonEmpty) =>
          if (l.contains(h._1)) {
            l -= h._1
            visited(h :: tail)
          } else visited(tail)
      }
    }
    visited((a.toList.zipWithIndex))
  }

  def maxProfit(A: Array[Int]) = {
    var maxProfit = 0
    var maxSlice = 0

    for (ind <- 1 until A.length) {
      maxSlice = math.max(0, maxSlice + (A(ind) - A(ind - 1))) // accumulated profit
      maxProfit = math.max(maxProfit, maxSlice)
    }
    maxProfit
  }

  def binaryGap(n: Int): Int = {
    val b = Integer.toBinaryString(n)
    b.split("1").dropRight(if (b.endsWith("0")) 1 else 0).foldLeft(0) {
      (acc, n) =>
      val a = n.count(_ == '0')
      if (a > acc) a
      else acc
    }
  }


  def cyclicRotation(n: Array[Int], k: Int) = {
    n.view.zipWithIndex.force.foldLeft[List[(Int, Int)]](List()){
      (acc, a) =>
        val s = (a._2 + k) % n.length
        val newIndex = if(s < 0) a._2 + k else s
        (a._1, newIndex) :: acc
    }.sortBy(_._2).map(_._1).toArray
  }

  def oddOccurrencesInArray(n: Array[Int]) = {
    /*n.groupBy(w => w).find(_._2.size == 1) match {
      case Some(x) => x._1
      case _ => -1
    }*/

    /*def findUnpaired(l: List[Int]): Int = {
      l match {
        case a::tail =>
          if ((l.count(_ == a) % 2) == 0)
            findUnpaired(tail.filterNot(_ == a))
          else a
      }
    }
    findUnpaired(n.toList)*/

    n.foldLeft(0) { (current, i) =>
      println("i = " + i + "  " + "Current " + current + " -- " + (i ^ current))
      i ^ current
    }
  }

  def dominator(n: Array[Int]) = {
    val s = n.length / 2
    n.view.zipWithIndex.force.groupBy(_._1).values.find(_.length > s) match {
      case Some(a) => a.map(_._2).mkString(",")
      case None => "-1"
    }

    /**
      * val s = n.length / 2
      * n.view.zipWithIndex.force.groupBy(_._1).values.filter(_.length > s) match {
      * case Nil => "-1"
      * case a => a.flatten.map(_._2).mkString(",")
      * }
      */
  }

  def frogJmp(x: Int, y: Int, d: Int) = {
    if (x == y) 0
    else {
      Math.ceil((y - x) / d.toDouble).toInt
    }
  }

  //https://codility.com/programmers/task/min_abs_sum_of_two/
  def minAbsSumOfTwo(A: Array[Int]): Int = {
      val withIndex = A.zipWithIndex
      val aa = for(x <- withIndex; y <- withIndex) yield (x, y)
      def findMax(arr: List[((Int, Int), (Int, Int))]): Int = {
        arr match {
          case Nil => 0
          case List(h) => Math.abs(h._1._1 + h._2._1)
          case h::m::tail =>
            if (Math.abs(h._1._1 + h._2._1) < Math.abs(m._1._1 + m._2._1)) {
              findMax(h :: tail)
            } else {
              findMax(m :: tail)
            }
        }
      }
      findMax(aa.toList)
  }


  def minAbsSumOfTwo2(A: Array[Int]): Int = {
    val sorted = A.sorted
    val size = A.length

    var left = 0
    var right = size - 1
    var minAbs = Math.abs(sorted(left) + sorted(right))

    while(left <= right) {
      val currentSum = sorted(left) + sorted(right)
      println(currentSum)
      minAbs = Math.min(minAbs, Math.abs(currentSum))

      if (currentSum <= 0)
        left = left + 1
      else right = right - 1
    }

    minAbs
  }

  def phoneBill(s: String): Int = {
    // prepare the duration and phoneNumber tuple list
    val tuples =  s.trim.split("\\n+").map { x =>
      x.trim.split(",") match {
        case Array(f, s) =>
          val numericalNum = s.replaceAll("-", "")
          val totalSeconds = f.split(":").map(_.trim.replaceFirst("^0+(?!$)", "")) match {
            case Array(q, w, e) => q.toInt * 60 * 60 + w.toInt * 60 + e.toInt
            case _ => 0
          }
          (totalSeconds, numericalNum)
        case _ => (0, "")
      }
    }

    tuples.groupBy(_._2) // group into map
      .map(m => (m._1, m._2.map(b => b._1).sum)) // calculate the sum of calls to same phone number
      .toList.sortBy(r => (-r._2, r._1)) match { // sort the smallest numerical phone number to the head in case of a tie
        case Nil => 0
        case List(x: (String, Int)) =>  0 // assume 1 log is always free of charge (always the longest duration)
        case h::tail =>
          tail.map { y =>
            val sec = y._2
            if (sec < 300) (3 * sec)
            else (150 * (Math.ceil(sec / 60.toDouble))).toInt // ceil to the nearest up value
          }.sum
    }
  }

  def fish(A: Array[Int], B: Array[Int]): Int = {
    val zipped = A.zip(B).toList

    @tailrec
    def helper(l: List[(Int, Int)], acc: List[Int]): List[Int] = {
      l match {
        case Nil => acc
        case List(x) => x._1 :: acc
        case (x, 1) :: (y, 0) :: tail =>
          if (x > y)
            helper((x, 1)::tail, acc)
          else
            helper((y, 0)::tail, acc)

        case x::y::tail => helper(y::tail, x._1 :: acc)
      }
    }

    helper(zipped, List()).size
  }

  def simulator(input: List[Int]) = {
    input.map { x =>
      x match {
        case _ if (x % 3 == 0 && x % 5 == 0) => "FizzBuzz"
        case _ if (x % 3 == 0) => "Fizz"
        case _ if (x % 5 == 0) => "Buzz"
        case y => y.toString
      }
    }
  }

}
