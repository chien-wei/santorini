package cs6963.santorini
import org.scalatest.{FlatSpec, Matchers}
import io.circe.syntax._

class BoardTest extends FlatSpec with Matchers {
  "Board.encodeJSON" should "create valid JSON String" in {
    val input = """{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"""
    val board = JSON.parseJSON(input)
    board.asJson.noSpaces shouldBe input
  }

  "Board.createActions" should "create action list" in {
    val board = JSON.parseJSON("{\"turn\":0,\"players\":[[[2,3],[4,1]],[[2,4],[5,3]]],\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]}")
    //board.createActions
  }

  /*"Game.moves" should "create boards after the moves" in {
    val boards = List(Board(12,List(Player(List(List(3, 2), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0))),
      Board(12,List(Player(List(List(1, 2), List(4, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0))),
    )
    val game = Game(boards)
    game.Moves.Builds.switchPlayerAndAddTurn.map(x => x.players(0)) shouldBe List
  }

  "Game.moves" should "create boards after the moves" in {
    val boards = List(Board(12,List(Player(List(List(3, 2), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0))),
      Board(12,List(Player(List(List(1, 2), List(4, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0))),
    )
    val game = Game(boards)
  }*/

  "Action.getDirs" should "create valid neighbors coordinates" in {
    Action.getDirs(List(3, 2)).toSet shouldBe Set(List(2, 2), List(3, 1), List(2, 1), List(4, 1), List(3, 3), List(4, 2), List(2, 3), List(4, 3))
    Action.getDirs(List(1, 1)).toSet shouldBe Set(List(1, 2), List(2, 1), List(2, 2))
    Action.getDirs(List(1, 5)).toSet shouldBe Set(List(1, 4), List(2, 4), List(2, 5))
    Action.getDirs(List(5, 1)).toSet shouldBe Set(List(4, 1), List(5, 2), List(4, 2))
    Action.getDirs(List(5, 5)).toSet shouldBe Set(List(4, 5), List(5, 4), List(4, 4))
    Action.getDirs(List(5, 3)).toSet shouldBe Set(List(4, 2), List(4, 4), List(5, 2), List(5, 4), List(4, 3))
  }

  "Action.getMoves" should "create valid result board" in {
    val board = Board(12,List(Player(List(List(3, 2), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)))
    Action.getMoves(List(3, 2), board).map(b => b.players).toSet shouldBe Set(
      List(Player(List(List(2, 2), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),
      List(Player(List(List(3, 1), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),
      List(Player(List(List(2, 1), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),
      List(Player(List(List(4, 1), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),
      List(Player(List(List(3, 3), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),
      List(Player(List(List(4, 2), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),
      List(Player(List(List(2, 3), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")))

    val board2 = Board(12,List(Player(List(List(3, 3), List(3, 2)),"Nocard"), Player(List(List(2, 4), List(4, 2)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)))
    Action.getMoves(List(3, 3), board2).map(b => b.players).toSet shouldBe Set(
      List(Player(List(List(2, 2), List(3, 2)),"Nocard"), Player(List(List(2, 4), List(4, 2)),"Nocard")), 
      List(Player(List(List(3, 4), List(3, 2)),"Nocard"), Player(List(List(2, 4), List(4, 2)),"Nocard")), 
      List(Player(List(List(4, 3), List(3, 2)),"Nocard"), Player(List(List(2, 4), List(4, 2)),"Nocard")), 
      List(Player(List(List(4, 4), List(3, 2)),"Nocard"), Player(List(List(2, 4), List(4, 2)),"Nocard")))
    // List(2, 3) is too high
  }

  "Action.getBuilds" should "create valid result board" in {
    val board = Board(12,List(Player(List(List(3, 2), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)))
    Action.getBuilds(List(3, 2), board).map(b => b.spaces).toSet shouldBe Set(
      List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 1, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)),
      List(List(0, 1, 0, 0, 0), List(1, 1, 2, 1, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)),
      List(List(0, 1, 1, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)),
      List(List(0, 1, 0, 1, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)),
      List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 2, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)),
      List(List(0, 2, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)),
      List(List(0, 1, 0, 0, 0), List(1, 2, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0)))
  }




}
