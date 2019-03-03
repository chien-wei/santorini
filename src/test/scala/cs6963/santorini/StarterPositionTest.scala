package cs6963.santorini
import org.scalatest.{FlatSpec, Matchers}

class StarterPositionTest extends FlatSpec with Matchers {

  "StarterPosition.random" should "generate random position from 2 PrePlayer" in {
    val players = StarterPosition.random(PrePlayer("Nocard"), PrePlayer("Nocard"))
    players._1 shouldBe PrePlayer("Nocard")
    players._2.card shouldBe "Nocard"
    players._2.tokens.flatten.filter(y => y > 4 || y < 2) shouldBe List()
  }

  "StarterPosition.random" should "generate random position from 1 PrePlayer and 1 Player" in {
    val players = StarterPosition.random(PrePlayer("Nocard"), Player(List(List(2, 1), List(3, 5)), "Nocard"))
    players._1 shouldBe Player(List(List(2, 1), List(3, 5)), "Nocard")
    players._2.card shouldBe "Nocard"
    players._2.tokens.flatten.filter(y => y > 4 || y < 2) shouldBe List()
  }
}