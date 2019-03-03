package cs6963.santorini

import scala.util.Random.nextInt

trait StarterPosition[A, B] {
  def random(player1: A, player2: A): (A, B)
}

object StarterPositionInstances {
  implicit val prePre: StarterPosition[PrePlayer, Player] = new StarterPosition[PrePlayer, Player] {
    override def random(player1: PrePlayer, player2: PrePlayer): (PrePlayer, Player) = {
      val token1 = List(nextInt(3) + 2, nextInt(3) + 2)
      var token2 = List(nextInt(3) + 2, nextInt(3) + 2)
      while (token1 == token2) {
        token2 = List(nextInt(3) + 2, nextInt(3) + 2)
      }
      //println((player2, Player(List(token1, token2), player1.card)))
      (player2, Player(List(token1, token2), player1.card))
    }
  }
}

object StarterPosition {
  def random[A, B](player1: A, player2: A)(implicit starterPosition: StarterPosition[A, B]) = {
    starterPosition.random(player1, player2)
  }
}
