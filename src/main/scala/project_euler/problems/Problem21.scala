package project_euler.problems

import project_euler.util.NumericTools

/**
  * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
  * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
  * *
  * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
  * *
  * Evaluate the sum of all the amicable numbers under 10000
  */

object Problem21 {

  def amicableSum(value: Long): Long = NumericTools.properDivisors(value).sum

  def main(args: Array[String]): Unit = {
    val BOUND = 10000
    val amicableMap = Stream.range(1, BOUND).map(x => (x.toLong, amicableSum(x))).toMap

    assert(amicableSum(220) == 284, "Sum failure: " + amicableSum(220) + " -> " + NumericTools.properDivisors(220))

    println(
      Stream.range(1, BOUND)
        .map(a => (a, amicableSum(a)))
        .filter(t => t._1 != t._2)
        .map(t => (t._1, amicableSum(t._2)))
        .filter(t => t._1 == t._2)
        .map(v => v._1)
        .sum
    )
  }
}
