package cs6963.santorini
import org.scalatest.{FlatSpec, Matchers}

class JSONTest extends FlatSpec with Matchers {
  "JSON parser" should "parse valid string" in {
    val board = JSON.parseJSON("""{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}""")
    board.turn shouldBe 18
    board.spaces shouldBe List(List(0, 0, 0, 0, 2), List(1, 1, 2, 0, 0), List(1, 0, 0, 3, 0), List(0, 0, 3, 0, 0), List(0, 0, 0, 1, 4))
    board.players shouldBe List(Player(List(List(2, 3), List(4, 4)),"Artemis"), Player(List(List(2, 5), List(3, 5)),"Prometheus"))
  }
}
