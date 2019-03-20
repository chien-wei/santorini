package cs6963.santorini
import scala.io.Source

object TurnCheck {

  def main(args: Array[String]): Unit = {
    //val filename = scala.io.StdIn.readLine() // "testcases.txt"

    var description = ""
    var str1 = ""
    var str2 = ""
    var line = ""
    var i = 0
    //for ((line, i) <- Source.fromFile(filename).getLines.zipWithIndex) {
    while (true) {
      line = scala.io.StdIn.readLine()
      if (line == "") sys.exit
      else if (i == 0) description = line
      else if (i == 1) str1 = line
      else if (i == 2) {
        str2 = line
        val board1 = JSON.parseJSON(str1)
        val board2 = JSON.parseJSON(str2)
        println(description)
        if (Cards.TurnCheck(board1, board2)) println("ok")
        else println("invalid")
      }
      i += 1
      i = i % 3
    }
  }
}
