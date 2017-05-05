package project_euler.problems

/*
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 */

object Problem6 {
  def square(x: Int) = x * x

  def square(x: BigInt) = x * x

  def main(args: Array[String]): Unit = {

    val sqrSum = Range.inclusive(1, 100).map(square(_)).sum
    val sumSqr = square(Range.inclusive(1, 100).sum)

    println(sumSqr - sqrSum)
  }
}