package project_euler.problems

/**
  * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
  * *
  * For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
  * *
  * What is the total of all the name scores in the file?
  */

class Problem22

object Problem22 {
  def nameScore(name: String, index: Int = 0): Long = {
    assert(name.matches("[A-Z]+"))
    name.toCharArray.map(c => c - 'A' + 1).sum
  }

  def main(args: Array[String]): Unit = {
    assert(nameScore("COLIN") == 53)
    val names = scala.io.Source.fromInputStream(classOf[Problem22].getResourceAsStream("/problem22/p022_names.txt"), "ASCII").getLines().mkString("\n")

    val opnames = names.split("\",\"").map(s => s match {
      case g: String if s.startsWith("\"") => g.substring(1)
      case g: String if s.endsWith("\"") => g.substring(0, g.length - 1)
      case _ => s
    })
      .map(s => {
        assert(s.matches("[A-Z]+"))
        s
      })
      .toList

    val sortednames = opnames.sorted
    val namesAndValues = sortednames.map(name => (name, nameScore(name))) //.map(t => SortedMap(t)).reduce((a, b) => a ++ b)
    val namesAndScores = namesAndValues.zipWithIndex.map(t => (t._1._1, t._1._2 * (t._2 + 1)))
    val sum = namesAndScores.map(t => t._2).sum
    println(
      sum
    )
  }
}
