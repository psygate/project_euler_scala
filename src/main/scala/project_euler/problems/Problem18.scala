package project_euler.problems

import scala.annotation.tailrec

/*
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)
 */
object Problem18 {
  val triangle = List(
    List(75),
    List(95, 64),
    List(17, 47, 82),
    List(18, 35, 87, 10),
    List(20, 4, 82, 47, 65),
    List(19, 1, 23, 75, 3, 34),
    List(88, 2, 77, 73, 7, 63, 67),
    List(99, 65, 4, 28, 6, 16, 70, 92),
    List(41, 41, 26, 56, 83, 40, 80, 70, 33),
    List(41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
    List(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
    List(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
    List(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
    List(63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
    List(4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23)
  )

  def max(a: Int, b: Int): Int = a match {
    case g: Int if g > b => g
    case g: Int if g < b => b
    case _ => a
  }

  def triangleSum(a: List[Int], b: List[Int]): List[Int] = {
    assert(a.length - b.length == -1 || a.length - b.length == 1 || a.length - b.length == 0)
    a.lengthCompare(b.length) match {
      case 0 => throw new IllegalArgumentException()
      case 1 => Stream.range(0, b.length).map(t => max(a(t) + b(t), a(t + 1) + b(t))).toList
      case -1 => triangleSum(b, a)
    }
  }

  @tailrec
  def collapseSum(list: List[List[Int]]): Int = {
    list.length match {
      case 1 => list(0).max
      case _ => {
        val last = list.last
        val prelast = list(list.length - 2)
        val listhead = list.zipWithIndex.filter(l => l._2 < list.length - 2).map(t => t._1)
        collapseSum(listhead ++ List(triangleSum(last, prelast)))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      collapseSum(triangle)
    )
  }
}
