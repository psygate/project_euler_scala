package project_euler.problems

import java.text.SimpleDateFormat
import java.util.Calendar

/**
  * You are given the following information, but you may prefer to do some research for yourself.
  * *
  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
  * April, June and November.
  * All the rest have thirty-one,
  * Saving February alone,
  * Which has twenty-eight, rain or shine.
  * And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
  * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
  */

object Problem19 {

  def main(args: Array[String]): Unit = {
    //1.1.1900 = Monday
    val cal = Calendar.getInstance()
    cal.set(1901, Calendar.JANUARY, 1)

    println(
      Stream.iterate(1)(x => x + 1)
        .map(x => {
          val l = cal.clone().asInstanceOf[Calendar]

          l.add(Calendar.MONTH, x)

          println(new SimpleDateFormat("dd.MM yyyy").format(l.getTime))
          l
        })
        .takeWhile(v => v.get(Calendar.YEAR) <= 2000)
        .filter(x => x.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY)
        .filter(x => x.get(Calendar.DAY_OF_MONTH) == 1)
        .map(x => 1)
        .sum
    )
  }
}
