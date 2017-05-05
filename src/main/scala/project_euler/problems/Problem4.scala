package project_euler.problems

/*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
 */

object Problem4 {
  def main(args: Array[String]): Unit = {
    println(
      Stream.range(100, 1000)
        .flatMap(n => Stream.range(100, n).map(t => n * t))
        .filter(t => t.toString == (t.toString.reverse))
        .max
    )
  }
}