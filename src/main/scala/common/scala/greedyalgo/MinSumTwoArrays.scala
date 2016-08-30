package common.scala.greedyalgo

/**
  * DP in C ++
  * int solution(vector<int> &A) {
    vector<int> P;
    int min = 20000 ;
    int dif = 0 ;
    P.resize(A.size()+1);
    P[0] = 0;
    for(int i = 1 ; i < P.size(); i ++)
    {
        P[i] = P[i-1]+A[i-1];

    }
    sort(P.begin(),P.end());
    for(int i = 1 ; i < P.size(); i++)
    {
         dif = P[i]-P[i-1];
         if(dif<min)
         {
             min = dif;
         }
    }
    return min;
}

  */
object MinSumTwoArrays extends App {

  def minAbsSumOfTwo2(A: Array[Int]): Int = {
    val sorted = A.sorted
    val size = A.length

    var left = 0
    var right = size - 1
    var minAbs = Math.abs(sorted(left) + sorted(right))

    while(left <= right) {
      val currentSum = sorted(left) + sorted(right)
      minAbs = Math.min(minAbs, Math.abs(currentSum))

      if (currentSum <= 0)
        left = left + 1
      else right = right - 1
    }

    minAbs
  }

  println(minAbsSumOfTwo2(Array(1, 5, 2, -2)))
}
