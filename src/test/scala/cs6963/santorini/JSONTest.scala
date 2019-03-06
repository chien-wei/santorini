package cs6963.santorini
import org.scalatest.{FlatSpec, Matchers}

class JSONTest extends FlatSpec with Matchers {
  "JSON.parseJSON" should "parse valid string" in {
    val board = JSON.parseJSON("""{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}""")
    board.turn shouldBe 18
    board.spaces shouldBe List(List(0, 0, 0, 0, 2), List(1, 1, 2, 0, 0), List(1, 0, 0, 3, 0), List(0, 0, 3, 0, 0), List(0, 0, 0, 1, 4))
    board.players shouldBe List(Player(List(List(2, 3), List(4, 4)),"Artemis"), Player(List(List(2, 5), List(3, 5)),"Prometheus"))
  }

  "JSON.parseJSON" should "parse valid no card string" in {
    val board = JSON.parseJSON("""{"turn":12,"players":[[[3,2],[5,1]],[[2,4],[4,3]]],"spaces":[[0,1,0,0,0],[1,1,2,0,0],[0,1,0,1,1],[0,0,1,1,0],[0,1,1,0,0]]}""")
    board.turn shouldBe 12
    board.spaces shouldBe List(List(0, 1, 0, 0, 0), List(1, 1, 2, 0, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0))
    board.players shouldBe List(Player(List(List(3, 2), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard"))
  }

  "JSON.parsePre" should "parse valid first no-card setup string" in {
    val preplayers = JSON.parsePre("""[]""")
    preplayers shouldBe (PrePlayer("Nocard"), PrePlayer("Nocard"))
  }

  "JSON.parsePre" should "parse valid second no-card setup string" in {
    val preplayers = JSON.parsePre("""[[[1,5],[2,2]]]""")
    preplayers shouldBe (PrePlayer("Nocard"), Player(List(List(1, 5), List(2, 2)),"Nocard"))
  }

  "JSON.parsePre" should "parse valid first setup string" in {
    val preplayers = JSON.parsePre("""[{"card":"Artemis"},{"card":"Prometheus"}]""")
    preplayers shouldBe (PrePlayer("Artemis"), PrePlayer("Prometheus"))
  }

  "JSON.parsePre" should "parse valid second setup string" in {
    val preplayers = JSON.parsePre("""[{"card":"Prometheus"},{"tokens":[[2,3],[4,4]],"card":"Artemis"}]""")
    preplayers shouldBe (PrePlayer("Prometheus"), Player(List(List(2, 3), List(4, 4)),"Artemis"))
  }

  "JSON.encode" should "encode valid board" in {
    val input = """{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"""
    val board = JSON.parseJSON(input)
    JSON.encode(board) shouldBe input
  }

  "JSON.encode" should "encode no card case during first interaction first player" in {
    val response = JSON.encode(PrePlayer("Nocard"), Player(List(List(3, 4), List(4, 3)),"Nocard"))
    response shouldBe "[[[3,4],[4,3]]]"
  }

  "JSON.encode" should "encode no card case during first interaction second player" in {
    val response = JSON.encode(Player(List(List(1, 2), List(2, 1)),"Nocard"), Player(List(List(3, 4), List(4, 3)),"Nocard"))
    response shouldBe "[[[1,2],[2,1]],[[3,4],[4,3]]]"
  }

  "JSON.encode" should "encode card case during first interaction first player" in {
    val response = JSON.encode(PrePlayer("Apollo"), Player(List(List(3, 4), List(4, 3)),"Artemis"))
    response shouldBe "[{\"card\":\"Apollo\"},{\"tokens\":[[3,4],[4,3]],\"card\":\"Artemis\"}]"
  }

  "JSON.encode" should "encode card case during first interaction second player" in {
    val response = JSON.encode(Player(List(List(1, 2), List(2, 1)),"Apollo"), Player(List(List(3, 4), List(4, 3)),"Artemis"))
    response shouldBe "[{\"tokens\":[[1,2],[2,1]],\"card\":\"Apollo\"},{\"tokens\":[[3,4],[4,3]],\"card\":\"Artemis\"}]"
  }

  "JSON.encode" should "encode no card case for board" in {
    val response = JSON.encode(Board(12,List(Player(List(List(4, 1), List(5, 1)),"Nocard"), Player(List(List(2, 4), List(4, 3)),"Nocard")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 1, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0))))
    response shouldBe "{\"turn\":12,\"players\":[[[4,1],[5,1]],[[2,4],[4,3]]],\"spaces\":[[0,1,0,0,0],[1,1,2,1,0],[0,1,0,1,1],[0,0,1,1,0],[0,1,1,0,0]]}"
  }

  "JSON.encode" should "encode card case for board" in {
    val response = JSON.encode(Board(12,List(Player(List(List(4, 1), List(5, 1)),"Apollo"), Player(List(List(2, 4), List(4, 3)),"Hephastus")),List(List(0, 1, 0, 0, 0), List(1, 1, 2, 1, 0), List(0, 1, 0, 1, 1), List(0, 0, 1, 1, 0), List(0, 1, 1, 0, 0))))
    response shouldBe "{\"turn\":12,\"players\":[{\"tokens\":[[4,1],[5,1]],\"card\":\"Apollo\"},{\"tokens\":[[2,4],[4,3]],\"card\":\"Hephastus\"}],\"spaces\":[[0,1,0,0,0],[1,1,2,1,0],[0,1,0,1,1],[0,0,1,1,0],[0,1,1,0,0]]}"
  }
}
