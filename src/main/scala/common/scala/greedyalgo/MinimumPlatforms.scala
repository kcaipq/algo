package common.scala.greedyalgo

/**
  * Created by kcai on 29/08/2016.
  *
  *
  * Given arrival and departure times of all trains that reach a railway station, find the minimum number of platforms required for the railway station so that no train waits.
  * We are given two arrays which represent arrival and departure times of trains that stop
  * *
  * Examples:
  * *
  * Input:  arr[]  = {9:00,  9:40, 9:50,  11:00, 15:00, 18:00}
  * dep[]  = {9:10, 12:00, 11:20, 11:30, 19:00, 20:00}
  * Output: 3
  * There are at-most three trains at a time (time between 11:00 to 11:20)
  *
  * We need to find the maximum number of trains that are there on the given railway station at a time. A Simple Solution is to take every interval one by one and find the number of intervals that overlap with it. Keep track of maximum number of intervals that overlap with an interval. Finally return the maximum value. Time Complexity of this solution is O(n2).
  * *
  * We can solve the above problem in O(nLogn) time. The idea is to consider all evens in sorted order. Once we have all events in sorted order, we can trace the number of trains at any time keeping track of trains that have arrived, but not departed.
  * *
  * For example consider the above example.
  * *
  * arr[]  = {9:00,  9:40, 9:50,  11:00, 15:00, 18:00}
  * dep[]  = {9:10, 12:00, 11:20, 11:30, 19:00, 20:00}
  * *
  * All events sorted by time.
  * Total platforms at any time can be obtained by subtracting total
  * departures from total arrivals by that time.
  * Time     Event Type     Total Platforms Needed at this Time
  * 9:00       Arrival                  1
  * 9:10       Departure                0
  * 9:40       Arrival                  1
  * 9:50       Arrival                  2
  * 11:00      Arrival                  3
  * 11:20      Departure                2
  * 11:30      Departure                1
  * 12:00      Departure                0
  * 15:00      Arrival                  1
  * 18:00      Arrival                  2
  * 19:00      Departure                1
  * 20:00      Departure                0
  * *
  * Minimum Platforms needed on railway station = Maximum platforms
  * needed at any time
  * = 3
  *
  *
  */
object MinimumPlatforms extends App {

  def findMinPlatformRequired(arr: List[Int], dep: List[Int], n: Int) = {
    val sortedArr = arr.sorted
    val sortedDep = dep.sorted

    var platformNeeded = 1
    var result = 1

    // traverse two lists
    var i = 1
    var j = 0

    while (i < n && j < n) {
      if (sortedArr(i) <= sortedDep(j)) {
        platformNeeded = platformNeeded + 1
        i = i + 1

        if (platformNeeded > result)
          result = platformNeeded
      } else {
        platformNeeded = platformNeeded - 1
        j = j + 1
      }
    }

    result
  }

  println(findMinPlatformRequired(List(900, 940, 950, 1100, 1500, 1800), List(910, 1200, 1120, 1130, 1900, 2000), 6))
  println(findMinPlatformRequired(List(900, 1000), List(1000, 1100), 2))
}
