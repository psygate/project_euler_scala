package project_euler.util

/**
  * Created by psygate on 05.05.2017.
  */
object NumericTools {

  class TriangleNumberGen() extends Iterator[Long] {
    var last = 0L
    var index = 0L

    override def hasNext: Boolean = true

    override def next(): Long = {
      index += 1
      last += index

      return last
    }
  }

  def triangleNumber(index: Long) = binomial(index + 1, 2)

  //    index match {
  //    case g: Long if g > 0 => Stream.range(1, index + 1).sum
  //    case _ => 1L
  //  }

  def divisors(number: Long) = List(1) ++ List(number) ++ Stream.range(2, number / 2 + 2).filter(d => number % d == 0).toList

  def numberOfDivisors(number: Long) = PrimeTools.primeFactorByFactorTuple(number).map(t => t._2 + 1).product

  def binomial(n: Long, k: Long): Long = k match {
    case 0L => 1L
    case g: Long if g == n => 1L
    case g: Long if n == 0 => 0L
    case _ => binomial(n - 1, k - 1) + binomial(n - 1, k)
  }

  def main(args: Array[String]): Unit = {
    println(Stream.range(1, 50).map(triangleNumber(_)).toList)

    println(Stream.range(1, 50).map(i => binomial(i + 1, 2)).toList)
    println(Stream.range(1, 50).map(i => binomial(i + 1, 2)).map(x => PrimeTools.primeFactorByFactorTuple(x)).toList)
    println(Stream.range(1, 50).map(i => binomial(i + 1, 2)).map(x => numberOfDivisors(x)).toList)
  }
}
