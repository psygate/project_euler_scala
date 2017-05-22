package project_euler.problems

import java.io.{BufferedWriter, File, FileInputStream, FileWriter}
import java.nio.file.{Files, Paths}

import project_euler.util.NumericTools

/**
  * A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
  * *
  * A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
  * *
  * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
  * *
  * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
  */

class Problem23

object Problem23 {
  val BOUND = 28123L
  val abundantNumbers = getAbundantNumbers(BOUND).sorted
  val abundantSums = Stream.range(0, abundantNumbers.length)
    .flatMap(i => Stream.range(i, abundantNumbers.length).map(j => abundantNumbers(i) + abundantNumbers(j)))
    .takeWhile(t => t <= BOUND)
    .toSet

  def getAbundantNumbers(BOUND: Long): List[Long] = {
    Files.exists(Paths.get("abundant.tmp")) match {
      case true => scala.io.Source.fromInputStream(new FileInputStream(new File("abundant.tmp"))).getLines().map(s => s.toLong).toList
      case false => {
        val abundantNumbers = (1L to BOUND).filter(n => isAbundandNumber(n)).toList
        val out = new BufferedWriter(new FileWriter(new File("abundant.tmp")))
        abundantNumbers.foreach(s => out.write(s.toString + "\n"))
        out.close()
        abundantNumbers
      }
    }
  }

  def isAbundandNumber(value: Long): Boolean = {
    value match {
      case g: Long if g < 12 => false
      case _ => {
        val divs = NumericTools.properDivisors(value).ensuring(x => !x.contains(value))
        divs.sum > value
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers = Stream.range(1, BOUND).filter(x => !abundantSums.contains(x))

    println(numbers)
    println(numbers.sum)
  }
}
