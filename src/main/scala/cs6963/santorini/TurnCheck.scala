package cs6963.santorini
import scala.io.Source

object TurnCheck {

  def main(args: Array[String]): Unit = {
    val filename = "testcases.txt"
    //val filename = "test.txt"
    var description = ""
    var str1 = ""
    var str2 = ""
    for ((line, i) <- Source.fromFile(filename).getLines.zipWithIndex) {
      if (i % 3 == 0) description = line
      else if (i % 3 == 1) str1 = line
      else if (i % 3 == 2) {
        str2 = line
        val board1 = JSON.parseJSON(str1)
        val board2 = JSON.parseJSON(str2)
        println(description)
        if (Cards.TurnCheck(board1, board2)) println("ok")
        else println("invalid")
      }
    }
  }
}
