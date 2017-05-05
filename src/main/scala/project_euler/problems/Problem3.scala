package project_euler.problems

/*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
 */

object Problem3 {
  def isPrime(value: Long): Boolean = {
    value match {
      case 1 => false
      case 2 => true
      case g: Long if (g & 1) == 0 => false
      case g: Long => !Stream.range(2, Math.sqrt(value).toLong).exists(value % _ == 0)
    }
  }

  def main(args: Array[String]): Unit = {
    val num = 600851475143L
    //    val primes = primesTo(Math.sqrt(num).toLong)

    println(
      Stream.range(2, Math.sqrt(num).toLong).filter(n => num % n == 0).filter(isPrime(_)).last
    )
  }
}