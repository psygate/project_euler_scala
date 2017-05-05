package project_euler.problems

import project_euler.util.PrimeTools

/*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
 */
object Problem10 {
  //  @tailrec
  //  def targetSumOfPrimes(sumLimit: Int, primeList: List[Int] = List.empty): Int = {
  //    primeList match {
  //      case g: List[Int] if g.isEmpty => targetSumOfPrimes(sumLimit, List(2))
  //      case g: List[Int] if g.sum > sumLimit => g.init.sum
  //      case g: List[Int] => targetSumOfPrimes(sumLimit, g ++ List(PrimeTools.nextPrime(g)))
  //    }
  //  }
  //
  //  //  def sumPrimes(primeLimit: Int): Int = {
  //  //    primeLimit match {
  //  //      case g: Int if g <= 1 => 0
  //  //      case 2 => 2
  //  //      case _ => sumPrimes(primeLimit, List.empty)
  //  //    }
  //  //  }
  //
  //  @tailrec
  //  def sumPrimes(primeLimit: Int, primeList: List[Int] = List.empty): Int = {
  //    primeLimit match {
  //      case g: Int if g <= 1 => 0
  //      case 2 => 2
  //      case _ => primeList match {
  //        case g: List[Int] if g.isEmpty => sumPrimes(primeLimit, g ++ List(PrimeTools.nextPrime(g)))
  //        case g: List[Int] if g.last > primeLimit => g.init.sum
  //        case g: List[Int] if g.last < primeLimit => sumPrimes(primeLimit, g ++ List(PrimeTools.nextPrime(g)))
  //      }
  //    }
  //  }

  def main(args: Array[String]): Unit = {
    val primes = PrimeTools.sieveL(Range.inclusive(0, 2000000).map(v => v.toLong).toList)
    //    assert(!primes.exists(!PrimeTools.isPrime(_)))
    println(primes.sum)
  }
}
