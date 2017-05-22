package project_euler.problems

import scala.annotation.tailrec

/**
  * A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
  * *
  * 1/2	= 	0.5
  * 1/3	= 	0.(3)
  * 1/4	= 	0.25
  * 1/5	= 	0.2
  * 1/6	= 	0.1(6)
  * 1/7	= 	0.(142857)
  * 1/8	= 	0.125
  * 1/9	= 	0.(1)
  * 1/10	= 	0.1
  * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
  * *
  * Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
  **/
object Problem26 {
  def longDivide(dividend: Int, divisor: Int, abortAfter: Int = 100): List[Int] = longDivide(dividend: Int, divisor: Int, List.empty, abortAfter)

  @tailrec
  private def longDivide(dividend: Int, divisor: Int, prev: List[Int], abortAfter: Int): List[Int] = {
    prev.length match {
      case x: Int if x == abortAfter => prev
      case _ => dividend % divisor match {
        case 0 => prev ++ List(dividend / divisor)
        case x: Int => longDivide(x * 10, divisor, prev ++ List(dividend / divisor), abortAfter)
      }
    }
  }

  def longDivideCycle(dividend: Int, divisor: Int): Int = longDivideCycle(dividend: Int, divisor: Int, List.empty)

  private def isCycle(list: List[Int]): Boolean = {
    list.length % 2 == 0 && !list.isEmpty match {
      case true => {
        (0 to list.length / 2 - 1).exists(i => list(i) != list(list.length / 2 + i))
      }
      case false => false
    }
  }

  @tailrec
  private def longDivideCycle(dividend: Int, divisor: Int, prev: List[Int]): Int = {
    dividend % divisor match {
      case 0 => 0
      case x: Int => isCycle(prev) match {
        case true => prev.length / 2
        case false => longDivideCycle(x * 10, divisor, prev ++ List(dividend / divisor))
      }
    }
  }

  def reciprocal(divisor: Int): List[Int] = reciprocal(1, divisor, List.empty)

  @tailrec
  private def reciprocal(dividend: Int, divisor: Int, previous: List[Int]): List[Int] = {
    dividend % divisor match {
      case 0 => List.empty
      case x: Int => reciprocal(x * 10, divisor, previous ++ List(dividend / divisor))
    }
  }

  def main(args: Array[String]): Unit = {
    println(longDivideCycle(1, 7))
    println(longDivide(1, 17))
  }
}
