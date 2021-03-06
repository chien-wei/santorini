package cs6963.santorini
import io.circe.syntax._
import io.circe.{ Decoder, JsonObject, ObjectEncoder }
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import scala.util.Random.nextInt

trait P {
  def card: String
}

@JsonCodec
case class PrePlayer(card: String) extends P

@JsonCodec
case class Player(tokens: List[List[Int]], card: String) extends P

@JsonCodec
case class Board(turn: Int, players: List[Player], spaces: List[List[Int]]) {

  //lazy val actions: List[Action] = validActions()

  def token0: List[Int] = this.players(0).tokens(0)
  def token1: List[Int] = this.players(0).tokens(1)
  def token2: List[Int] = this.players(1).tokens(0)
  def token3: List[Int] = this.players(1).tokens(1)

  def isWin: Boolean = {
    val spaceTmp = this.spaces.flatten.toArray
    if (spaceTmp(token0(1) + 5 * token0(0) - 6) == 3 || spaceTmp(token1(1) + 5 * token1(0) - 6) == 3) true
    else false
  }


  def addChange(change: Change): Board = {
    val spaces = (this.spaces.flatten, change.spaces.flatten).zipped.map(_+_).grouped(5).toList
    val token0 = (this.token0, change.tokens(0)).zipped.map(_+_)
    val token1 = (this.token1, change.tokens(1)).zipped.map(_+_)
    val token2 = (this.token2, change.tokens(2)).zipped.map(_+_)
    val token3 = (this.token3, change.tokens(3)).zipped.map(_+_)
    val newBoard = Board(this.turn,
      List(Player(List(token0, token1), this.players(0).card), Player(List(token2, token3), this.players(1).card)),
      spaces)
    newBoard
  }
}

object Action {

  def getDirs(pos: List[Int]): List[List[Int]] = {
    val (x, y) = (pos(0), pos(1))
    List(List(x-1, y), List(x, y-1), List(x-1, y-1), List(x+1, y-1),
      List(x, y+1), List(x+1, y), List(x-1, y+1), List(x+1, y+1)).
      filter(p => !(p(0) < 1 || p(0) > 5 || p(1) < 1 || p(1) > 5))
  }

  def getMoves(token: List[Int], board: Board): List[Board] = {

    val height = Board.getHeight(board, token)
    val dirs = getDirs(token)

    val heights = dirs.map(x => Board.getHeight(board, x))
    // check height
    //val moveable = dirs.zip(heights).filter(x => x._2 <= height+1 && x._2 <= 4).map(x => x._1)
    val token2 = if (board.players(0).tokens(0) == token) board.players(0).tokens(1) else board.players(0).tokens(0)
    val token3 = board.players(1).tokens(0)
    val token4 = board.players(1).tokens(1)
    val moveable = dirs.zip(heights).filter(x => x._2 <= height+1 && x._2 <= 4).map(x => x._1)
      .filter(x => x != token2 && x != token3 && x != token4)
    // generate board
    val two = board.players(0).tokens
    //println(two)

    val newBoards: List[Board] = {
      moveable.map(x => {
        val newTwo = if (two(0) == token) List(x, two(1))
        else List(two(0), x)
        Board(board.turn, List(Player(newTwo, board.players(0).card), board.players(1)), board.spaces)
      })
    }
    newBoards
  }
  def getBuilds(token: List[Int], board: Board): List[Board] = {

    val height = Board.getHeight(board, token)
    val dirs = getDirs(token)

    val heights = dirs.map(x => Board.getHeight(board, x))
    // check height
    //val buildable = dirs.zip(heights).filter(x => x._2 <= 3).map(x => x._1)
    val token2 = if (board.players(0).tokens(0) == token) board.players(0).tokens(1) else board.players(0).tokens(0)
    val token3 = board.players(1).tokens(0)
    val token4 = board.players(1).tokens(1)
    val buildable = dirs.zip(heights).filter(x => x._2 <= height+1 && x._2 <= 4).map(x => x._1)
      .filter(x => x != token2 && x != token3 && x != token4)
    // generate board
    val two = board.players(0).tokens
    //println(two)

    //println(buildable)
    val newBoards: List[Board] = {
      buildable.map(x => {
        val array = board.spaces.flatten.toArray
        //println(x(0), x(1))
        array(x(0) + 5 * x(1) - 6) += 1
        val newSpace = array.toList.grouped(5).toList
        Board(board.turn, board.players, newSpace)
      })
    }
    newBoards
  }
}


object Board {

  def handleFirst(prePlayers: Any): String = {
    val(player1, player2) = prePlayers
    player2 match {
      case p: PrePlayer => println("1")
      case p: Player => println("2")
    }
    ""
  }

  def getHeight(board: Board, pos: List[Int]): Int = {
    board.spaces.flatten.toList(pos(1) + 5 * pos(0) - 6)
  }

  def switchPlayerAndAddTurn(board: Board): Board = {
    Board(board.turn+1, List(board.players(1), board.players(0)), board.spaces)
  }
}

/*object tmp extends App {
  val board = JSON.parseJSON("""{"turn":12,"players":[[[3,2],[5,1]],[[2,4],[4,3]]],"spaces":[[0,1,0,0,0],[1,1,2,0,0],[0,1,0,1,1],[0,0,1,1,0],[0,1,1,0,0]]}""")
  // token 0
  val token0 = List(board.players(0).tokens(0)(0), board.players(0).tokens(0)(1))
  // token 1
  val token1 = List(board.players(0).tokens(1)(0), board.players(0).tokens(1)(1))
  println(board)
  val boards = Action.getMoves(token0, board).map(b => Action.getBuilds(token0, b)).flatten ++
    Action.getMoves(token1, board).map(b => Action.getBuilds(token1, b)).flatten
  println(JSON.encode(Board.switchPlayerAndAddTurn(boards(nextInt(boards.length)))))
  val b = boards(nextInt(boards.length))
  println(b)
  println(JSON.encode(b))

}*/

