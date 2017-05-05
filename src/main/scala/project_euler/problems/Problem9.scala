package project_euler.problems

/**
  * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
  * *
  * a2 + b2 = c2
  * For example, 32 + 42 = 9 + 16 = 25 = 52.
  * *
  * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  * Find the product abc.
  */

case class Duplet[T: Numeric](val a: T, val b: T) {

}

case class Triplet[T: Numeric](val a: T, val b: T, val c: T) {

}


object Problem9 {
  def product(t: Triplet[Int]) = t.a * t.b * t.c


  def main(args: Array[String]): Unit = {
    println(
      product(
      Range.inclusive(1, 1000)
        .flatMap(c => Range.inclusive(1, c).map(b => Duplet(b, c)))
        .filter(bc => 1000 - bc.b - bc.a < bc.a && 1000 - bc.b - bc.a < bc.b && 1000 - bc.a - bc.b > 0)
        .map(bc => Triplet(1000 - bc.a - bc.b, bc.a, bc.b))
        .filter(t => t.a + t.b + t.c == 1000)
        .filter(t => t.a * t.a + t.b * t.b == t.c * t.c)
        .head
      )
    )
  }
}
