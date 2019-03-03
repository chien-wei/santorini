package cs6963.santorini

import scala.util.Random.nextInt

// TODO: There should be a better way to do this.
trait StarterPosition[A, B] {
  def random(player1: A, player2: A): (A, B)
  def random(player1: A, player2: B)(implicit di: DummyImplicit): (B, B)
}

object StarterPosition extends StarterPosition[PrePlayer, Player] {
  def random(player1: PrePlayer, player2: PrePlayer): (PrePlayer, Player) = {
    val token1 = List(nextInt(3) + 2, nextInt(3) + 2)
    var token2 = List(nextInt(3) + 2, nextInt(3) + 2)
    while (token1 == token2) {
      token2 = List(nextInt(3) + 2, nextInt(3) + 2)
    }
    (player2, Player(List(token1, token2), player1.card))
  }

  def random(player1: PrePlayer, player2: Player)(implicit di: DummyImplicit): (Player, Player) = {
    val token1 = player2.tokens(0)
    val token2 = player2.tokens(1)
    var token3 = List(nextInt(3) + 2, nextInt(3) + 2)
    while (token3 == token1 || token3 == token2) {
      token3 = List(nextInt(3) + 2, nextInt(3) + 2)
    }
    var token4 = List(nextInt(3) + 2, nextInt(3) + 2)
    while (token4 == token3 || token4 == token1 || token3 == token2) {
      token4 = List(nextInt(3) + 2, nextInt(3) + 2)
    }
    (player2, Player(List(token3, token4), player1.card))
  }
}