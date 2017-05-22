package project_euler.util

/**
  * Created by psygate on 06.05.2017.
  */

object PermutationUtil {
  def permutation(string: String): List[String] = permutation("", string)

  private def permutation(prefix: String, str: String): List[String] = {
    str.length match {
      case 0 => List(prefix)
      case _ => (0 to str.length - 1).flatMap(i => permutation(prefix + str.charAt(i), str.substring(0, i) + str.substring(i + 1, str.length))).toList
    }
  }
}
