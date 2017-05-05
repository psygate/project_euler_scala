package project_euler.problems

/*
      Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

    By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
 */

object Problem2 {
  def fibonacci(n: Int): Int = {
    n match {
      case 0 | 1 => 1
      case n: Int => fibonacci(n - 1) + fibonacci(n - 2)
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      Stream.range(0, 1000).map(v => fibonacci(v)).takeWhile(_ < 4000000).filter(n => (n & 1) == 0).sum
    )
  }
}