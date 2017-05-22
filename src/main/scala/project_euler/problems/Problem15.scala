package project_euler.problems

import project_euler.util.NumericTools

/*
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


How many such routes are there through a 20×20 grid?
 */

/*
https://en.wikipedia.org/wiki/Lattice_path
 */
object Problem15 {


  def main(args: Array[String]): Unit = {
    println(
      NumericTools.binomialCoefficient(20 + 20, 20)
    )
  }
}
