package cs6963.santorini

import scala.util.Random.nextInt
import scala.util.Random.shuffle
import io.circe.generic.JsonCodec
import io.circe.syntax._


object Santorini {

  def generateMoveAndBuild(hash: Map[String, String]): List[String] = {

    val dirs = List(List(0, -1), List(-1, -1), List(-1, 0), List(-1, 1),
      List(0, 1), List(1, 1), List(1, 0), List(1, -1))
    val spaces = hash("spaces").slice(2, hash("spaces").length-2).split("\\],\\[").toList
      .map(ss => ss.split(",").toList.map(c => c.toInt))
    val turn = hash("turn").toInt
    val workers = hash("players")
    val worker1 = workers.slice(3,6).split(",").map(s => s.toInt).toList
    val worker2 = workers.slice(9,12).split(",").map(s => s.toInt).toList
    val worker3 = workers.slice(17,20).split(",").map(s => s.toInt).toList
    val worker4 = workers.slice(23,26).split(",").map(s => s.toInt).toList

    def getResults(worker: List[Int]): List[String] = {

      val curHeight = spaces(worker(0)-1)(worker(1)-1)
// fold
      def getValidMoves(i: Int): List[List[Int]] = {
        if (i < dirs.length) {
          val newPos = worker.zip(dirs(i)).map({ case (x, y) => x + y })
          if (newPos(0) < 1 || newPos(0) > 5 || newPos(1) < 1 || newPos(1) > 5) getValidMoves(i + 1)
          else {
            val newHeight = spaces(newPos(0) - 1)(newPos(1) - 1)

            // not allow short circuit in this class
            if (curHeight == 2 && newHeight == 3) {
              val notMoved = if (worker == worker1) worker2 else worker1
              println("{\"turn\":" + (turn+1).toString +
                ",\"players\":" + "[[[" + worker3.mkString(",") + "],[" + worker4.mkString(",") + "]],[[" +
                newPos.mkString(",") + "],[" + notMoved.mkString(",") + "]]],\"spaces\":" +
                "[" + spaces.map(row => "[" + row.mkString(",") + "]").mkString(",") + "]}")
              //System.err.println("I win!")
              System.exit(0)
            }

            if ((worker == worker1 && newPos == worker2) || (worker == worker2 && newPos == worker1) ||
              newPos == worker3 || newPos == worker4 || newHeight == 4 || newHeight > curHeight + 1) getValidMoves(i + 1)
            else List(newPos) ::: getValidMoves(i + 1)
          }
        }
        else List()
      }

      val moves = getValidMoves(0)

      def generateJsonString(movesIndex: Int, newSpaces: Array[Array[Int]]): String = {
        val notMoved = if (worker == worker1) worker2 else worker1
        "{\"turn\":" + (turn+1).toString +
          ",\"players\":" + "[[[" + worker3.mkString(",") + "],[" + worker4.mkString(",") + "]],[[" +
          moves(movesIndex).mkString(",") + "],[" + notMoved.mkString(",") + "]]],\"spaces\":" +
          "[" + newSpaces.map(row => "[" + row.mkString(",") + "]").mkString(",") + "]}"
      }

      def getValidResults(movesIndex: Int, dirsIndex: Int): List[String] = {
        if (dirsIndex >= dirs.length) List()
        else if (movesIndex == moves.length) getValidResults(0, dirsIndex+1)
        else {
          val buildPos = moves(movesIndex).zip(dirs(dirsIndex)).map( { case (x, y) => x + y })
          if (buildPos(0) < 1 || buildPos(0) > 5 || buildPos(1) < 1 || buildPos(1) > 5)
            getValidResults(movesIndex+1, dirsIndex)
          else {
            var newSpaces = spaces.map(row => row.toArray).toArray
            newSpaces(buildPos(0) - 1)(buildPos(1) - 1) += 1

            val buildHeight = newSpaces(buildPos(0) - 1)(buildPos(1) - 1)
            if ((worker == worker1 && buildPos == worker2) || (worker == worker2 && buildPos == worker1) ||
              buildPos == worker3 || buildPos == worker4 || buildHeight > 4) getValidResults(movesIndex + 1, dirsIndex)
            else List(generateJsonString(movesIndex, newSpaces)) ::: getValidResults(movesIndex + 1, dirsIndex)
          }
        }
      }

      getValidResults(0, 0)
    }

    getResults(worker1) ::: getResults(worker2)
  }

  def main(args: Array[String]): Unit = {

    // []
    // [[[1,5],[2,2]]]
    // [{"card":"Artemis"},{"card":"Prometheus"}]
    // [{"card":"Prometheus"},{"tokens":[[2,3],[4,4]],"card":"Artemis"}]
    val firstInputString = scala.io.StdIn.readLine()
    val (player1, player2) = JSON.parsePre(firstInputString)

    // TODO: get type mismatch
    // val res = StarterPosition.random(player1, player2)
    val firstRespond = player2 match {
      case p:PrePlayer => {
        val res = StarterPosition.random(player1.asInstanceOf[PrePlayer], player2.asInstanceOf[PrePlayer])
        JSON.encode(res._1, res._2)
      }
      case p:Player => {
        val res = StarterPosition.random(player1.asInstanceOf[PrePlayer], player2.asInstanceOf[Player])
        JSON.encode(res._1, res._2)
      }
    }
    println(firstRespond)


    while (true) {
      val board = JSON.parseJSON(scala.io.StdIn.readLine())
      // token 0
      val token0 = board.players(0).tokens(0)
      // token 1
      val token1 = board.players(0).tokens(1)
      println(JSON.encode(shuffle(Cards.actions(token0, board)).head))
    }

  }
}