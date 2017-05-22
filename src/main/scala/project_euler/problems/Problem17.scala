package project_euler.problems

/*
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
 */
object Problem17 {
  def base = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine"
  )

  def teen = Map(
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eightteen",
    19 -> "ninteen"
  );

  def ten = Map(
    2 -> "twenty",
    3 -> "thirty",
    4 -> "forty",
    5 -> "fifty",
    6 -> "sixty",
    7 -> "seventy",
    8 -> "eighty",
    9 -> "ninety"
  )

  def num2word(value: Int): String = {
    value match {
      case g: Int if g < 10 => base(g)
      case g: Int if g < 20 => teen(g)
      case g: Int if g < 100 => g match {
        case n: Int if n % 10 == 0 => ten(n / 10)
        case n: Int => ten(n / 10) + "-" + num2word(n % 10)
        case _ => ""
      }
      case g: Int if g < 1000 => g match {
        case n: Int if n % 100 == 0 => num2word(g / 100) + " hundred"
        case n: Int => num2word(g / 100) + " hundred and " + num2word(g % 100)
      }
      case g: Int if g == 1000 => "one thousand"
      case _ => ""
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      Stream.range(1, 1000 + 1).map(v => num2word(v))
        .map(s => {
          println(s)
          s
        })
        .map(s => s.replaceAll("(\\s|-)", ""))
        .map(s => s.length)
        .sum
    )
  }
}
