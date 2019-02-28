package cs6963.santorini
import org.scalatest.{FlatSpec, Matchers}
import io.circe.syntax._

class BoardTest extends FlatSpec with Matchers {
  "Board.encodeJSON" should "create valid JSON String" in {
    val input = """{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"""
    val board = JSON.parseJSON(input)
    board.asJson.noSpaces shouldBe input
  }
}
