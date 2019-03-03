package cs6963.santorini
import org.scalatest.{FlatSpec, Matchers}

class StarterPositionTest extends FlatSpec with Matchers {
  import StarterPositionInstances._
  "StarterPosition.random" should "generate random position from 2 PrePlayer" in {
    val players = StarterPosition.random[PrePlayer, Player](PrePlayer("Nocard"), PrePlayer("Nocard"))
    println(players)
    players._1 shouldBe PrePlayer("Nocard")
    players._2.card shouldBe "Nocard"
    players._2.tokens.flatten.filter(y => y > 4 || y < 2) shouldBe List()
  }
}