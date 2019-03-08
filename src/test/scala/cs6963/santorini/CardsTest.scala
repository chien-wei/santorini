package cs6963.santorini

import org.scalatest.{FlatSpec, Matchers}

class CardsTest extends FlatSpec with Matchers {
  "Cards.action" should "create valid JSON String" in {
    val input = """{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"""
    val board = JSON.parseJSON(input)
  }

  "Change.add" should "add Change to Change" in {
    val ch1 = Change(List(List(0, 1), List(0, 0), List(5, 2), List(1, 0)),
      List(List(0, 0, 0, 0, 0), List(0, 1, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 1), List(0, 0, 0, 0, 0)))
    val ch2 = Change(List(List(0, 1), List(0, 0), List(0, 0), List(0, 0)),
      List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 4, 0), List(0, 0, 0, 0, 0)))
    ch1.add(ch2) shouldBe Change(List(List(0, 2), List(0, 0), List(5, 2), List(1, 0)),
      List(List(0, 0, 0, 0, 0), List(0, 1, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 4, 1), List(0, 0, 0, 0, 0)))
  }
  "Change.buildOn" should "build one level on index" in {
    val ch1 = Change(List(List(0, 1), List(0, 0), List(5, 2), List(1, 0)),
      List(List(1, 2, 3, 0, 0), List(0, 1, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 1), List(0, 0, 0, 0, 0)))
    ch1.buildOn(List(1,2)) shouldBe
      Change(List(List(0, 1), List(0, 0), List(5, 2), List(1, 0)),
        List(List(1, 3, 3, 0, 0), List(0, 1, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 1), List(0, 0, 0, 0, 0)))
  }
}
