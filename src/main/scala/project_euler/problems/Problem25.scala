package project_euler.problems

import scala.annotation.tailrec

/**
  * The Fibonacci sequence is defined by the recurrence relation:
  * *
  * Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
  * Hence the first 12 terms will be:
  * *
  * F1 = 1
  * F2 = 1
  * F3 = 2
  * F4 = 3
  * F5 = 5
  * F6 = 8
  * F7 = 13
  * F8 = 21
  * F9 = 34
  * F10 = 55
  * F11 = 89
  * F12 = 144
  * The 12th term, F12, is the first term to contain three digits.
  * *
  * What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
  **/
object Problem25 {

  def fibonacci(index: Long): BigInt = return fibonacciHelp(0, 1, index)

  @tailrec
  private def fibonacciHelp(a: BigInt, b: BigInt, n: Long): BigInt = {
    n match {
      case g: Long if g > 0 => fibonacciHelp(b, a + b, n - 1)
      case _ => a
    }
  }

  def fibonacci(condition: (BigInt, Long) => Boolean): BigInt = return fibonacciHelp(0, 1, 0L, condition)

  @tailrec
  private def fibonacciHelp(a: BigInt, b: BigInt, count: Long, condition: (BigInt, Long) => Boolean): BigInt = {
    condition(a, count) match {
      case false => fibonacciHelp(b, a + b, count + 1, condition)
      case true => a
    }
  }

  def main(args: Array[String]): Unit = {
    fibonacci((a, c) => a.toString match {
      case s: String if s.length == 1000 => {
        println(c)
        println(s.length)
        true
      }
      case _ => false
    })
  }
}
