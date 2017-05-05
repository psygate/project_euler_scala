package project_euler.util

import scala.annotation.tailrec

/**
  * Created by psygate on 04.05.2017.
  */


object PrimeTools {
  type Number >: Numeric[_]

  def isPrimeRespective(x: Int, primes: List[Int]): Boolean = !primes.exists(p => x % p == 0)

  def isPrime(x: Int): Boolean = x >= 2 && !primesTo(x).exists(v => x != v && x % v == 0)

  def primesTo(x: Int, hints: List[Int] = List.empty): List[Int] = {
    x match {
      case g: Int if g <= 2 => List(2)
      case g: Int => {
        val primes = primesTo(g - 1)
        isPrimeRespective(g, primes) match {
          case false => primes
          case true => primes ++ List(g)
        }
      }
    }
  }

  @tailrec
  def nextPrime(x: Int, primes: List[Int] = List.empty): Int = {
    val localprimes = primes match {
      case g: List[Int] if g.isEmpty => primesTo(x)
      case g: List[Int] => g
    }

    val delta = x match {
      case g: Int if g % 2 == 0 => 1
      case _ => 2
    }

    isPrimeRespective(x + delta, localprimes) match {
      case true => x + delta
      case false => nextPrime(x + delta, localprimes)
    }
  }

  def nextPrime(primes: List[Int]): Int = primes match {
    case g: List[Int] if g.isEmpty => 2
    case g: List[Int] => nextPrime(primes.head, primes)
  }

  def primeList(length: Int, primes: List[Int] = List.empty): List[Int] = {
    length match {
      case g: Int if g <= 0 => List()
      case g: Int if g == 1 => List(2)
      case _ => {
        val localprimes = primeList(length - 1)
        primes ++ List(nextPrime(primes.last, primes))
      }
    }
  }

  def sieve(input: List[Int]): List[Int] = {
    val list = input.filter(l => l >= 2)
    assert(list.head == 2)
    sieve(list, 0)
  }

  @tailrec
  private def sieve(list: List[Int], index: Int): List[Int] = {
    list match {
      case g: List[Int] if g.isEmpty => List.empty
      case g: List[Int] if g.length == index => g
      case g: List[Int] => {
        val (head, tail) = g.splitAt(index)
        val num = g(index)
        num match {
          case q: Int if q >= Math.sqrt(list.last) + 1 => list
          case _ =>
            sieve(head ++ List(num) ++ tail.filter(n => n % num != 0), index + 1)
        }
      }
    }
  }

  def sieveL(input: List[Long]): List[Long] = {
    val list = input.filter(l => l >= 2)
    assert(list.head == 2)
    sieveL(list, 0)
  }

  @tailrec
  private def sieveL(list: List[Long], index: Int): List[Long] = {
    list match {
      case g: List[Long] if g.isEmpty => List.empty
      case g: List[Long] if g.length == index => g
      case g: List[Long] => {
        val (head, tail) = g.splitAt(index)
        val num = g(index)
        num match {
          case q: Long if q >= Math.sqrt(list.last) + 1 => list
          case _ =>
            sieveL(head ++ List(num) ++ tail.filter(n => n % num != 0), index + 1)
        }
      }
    }
  }

  def primeFactor(number: Long, factor: Long = 2L): List[Long] = {
    number match {
      case 1 => List.empty
      case n: Long => {
        n % factor match {
          case 0 => List(factor) ++ primeFactor(n / factor)
          case _ => primeFactor(n, factor + 1)
        }
      }
    }
  }

  def primeFactorByFactorTuple(number: Long): List[Tuple2[Long, Long]] = primeFactor(number).groupBy(f => f).map(m => (m._1, m._2.length.toLong)).toList

  class PrimeGenerator() extends Iterator[Int] {
    var primesList: List[Int] = List.empty

    override def hasNext: Boolean = true

    override def next(): Int = {
      primesList.isEmpty match {
        case true => primesList ++= List(2)
        case false => primesList ++= List(nextPrime(primesList.last, primesList))
      }

      primesList.last
    }
  }

}
