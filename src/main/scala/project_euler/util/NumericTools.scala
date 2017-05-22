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

  def divisors(number: Long) = number match {
    case g: Long if g < 0 => throw new IllegalArgumentException("Value smaller 0 is not allowed.")
    case 0 => throw new IllegalArgumentException("Proper divisors of 0 are infinite.")
    case 1 => List(1L)
    case _ => properDivisors(number) ++ List(number)
  }

  def properDivisors(number: Long) = number match {
    case g: Long if g < 0 => throw new IllegalArgumentException("Value smaller 0 is not allowed.")
    case 0 => throw new IllegalArgumentException("Proper divisors of 0 are infinite.")
    case 1 => List(1L)
    case 2 => List(1L)
    case _ => List(1L) ++ Stream.range(2, number / 2 + 2).filter(d => number % d == 0).toList
  }


  def numberOfDivisors(number: Long) = number match {
    case g: Long if g < 0 => throw new IllegalArgumentException("Value smaller 0 is not allowed.")
    case 0 => throw new IllegalArgumentException("Proper divisors of 0 are infinite.")
    case 1 => 1L
    case _ => PrimeTools.primeFactorByFactorTuple(number).map(t => t._2 + 1).product
  }

  def binomial(n: Long, k: Long): Long = k match {
    case 0L => 1L
    case g: Long if g == n => 1L
    case g: Long if n == 0 => 0L
    case _ => binomial(n - 1, k - 1) + binomial(n - 1, k)
  }

  def factorial(n: BigInt): BigInt = {
    n match {
      case ONE => ONE
      case _ => n * factorial(n - ONE)
    }
  }

  def binomialCoefficient(n: Int, k: Int) =
    (BigInt(n - k + 1) to n).product /
      (BigInt(1) to k).product

  private val ZERO = BigInt(0)
  private val ONE = BigInt(1)

  def binomial(n: BigInt, k: BigInt): BigInt = k match {
    case ZERO => 1L
    case g: BigInt if g == n => ONE
    case g: BigInt if n == ZERO => ZERO
    case _ => binomial(n - 1, k - 1) + binomial(n - 1, k)
  }

  def main(args: Array[String]): Unit = {
    println(Stream.range(1, 50).map(triangleNumber(_)).toList)

    println(Stream.range(1, 50).map(i => binomial(i + 1, 2)).toList)
    println(Stream.range(1, 50).map(i => binomial(i + 1, 2)).map(x => PrimeTools.primeFactorByFactorTuple(x)).toList)
    println(Stream.range(1, 50).map(i => binomial(i + 1, 2)).map(x => numberOfDivisors(x)).toList)
  }
}
