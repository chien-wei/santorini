package cs6963.santorini

import scala.util.Random.nextInt

// TODO: There should be a better way to do this.
trait StarterPosition[A, B] {
  def random1(player1: A, player2: A): (A, B)
  def random2(player1: A, player2: B): (B, B)
}

object StarterPositionInstances {
  implicit val prePre: StarterPosition[PrePlayer, Player] = new StarterPosition[PrePlayer, Player] {
    override def random1(player1: PrePlayer, player2: PrePlayer): (PrePlayer, Player) = {
      val token1 = List(nextInt(3) + 2, nextInt(3) + 2)
      var token2 = List(nextInt(3) + 2, nextInt(3) + 2)
      while (token1 == token2) {
        token2 = List(nextInt(3) + 2, nextInt(3) + 2)
      }
      (player2, Player(List(token1, token2), player1.card))
    }

    override def random2(player1: PrePlayer, player2: Player): (Player, Player) = {
      val token1 = List(nextInt(3) + 2, nextInt(3) + 2)
      var token2 = List(nextInt(3) + 2, nextInt(3) + 2)
      while (token1 == token2) {
        token2 = List(nextInt(3) + 2, nextInt(3) + 2)
      }
      (player2, Player(List(token1, token2), player1.card))
    }
  }
}

object StarterPosition {
  def random1[A, B](player1: A, player2: A)(implicit starterPosition: StarterPosition[A, B]) = {
    starterPosition.random1(player1, player2)
  }
  def random2[A, B](player1: A, player2: B)(implicit starterPosition: StarterPosition[A, B]) = {
    starterPosition.random2(player1, player2)
  }
}
