package project_euler.problems

/*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */

object Problem5 {
  def primeFactors(n: Int, factor: Int): List[Int] = {
    n match {
      case 1 => List.empty
      case g: Int if g % factor == 0 => List(factor) ++ primeFactors(g / factor)
      case _ => primeFactors(n, factor + 1)
    }
  }

  def primeFactors(n: Int): List[Int] = n match {
    case 1 => List(1)
    case _ => primeFactors(n, 2)
  }

  def main(args: Array[String]): Unit = {
    val product = Range.inclusive(1, 20)
      .flatMap(v => primeFactors(v).groupBy(v => v))
      .map(t => t._2)
      .distinct
      .groupBy(_.head)
      .map(v => v._2.reduce((a, b) => a.lengthCompare(b.length) match {
        case 1 => a
        case 0 => a
        case -1 => b
      }))
    .flatMap(v => v)
    .product

    println(product)
  }
}