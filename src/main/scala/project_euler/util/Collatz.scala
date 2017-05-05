package project_euler.util

import scala.annotation.tailrec

/**
  * Created by psygate on 05.05.2017.
  */
object Collatz {
  val ONE = BigInt(1)

  @tailrec
  def collatzLength(n: BigInt, it: Int = 0): Int = {
    n match {
      case ONE => 1 + it
      case g: BigInt if (g & ONE) == 0 => collatzLength(g / 2, it + 1)
      case g: BigInt => collatzLength(3 * g + 1, it + 1)
    }
  }
}
