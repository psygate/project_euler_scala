package project_euler.problems

import project_euler.util.PrimeTools.PrimeGenerator

/*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
 */

object Problem7 {
  def main(args: Array[String]): Unit = {
    val pg = new PrimeGenerator()
    println(Stream.range(0, 10001).map(v => pg.next()).last)
  }
}