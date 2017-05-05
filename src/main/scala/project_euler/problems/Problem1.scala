package project_euler.problems

object Problem1 {
  def main(args: Array[String]): Unit = {
    println(
      Stream.range(0,1000).filter(n => (n % 5 == 0) || (n % 3 == 0)).sum
    )
  }
}