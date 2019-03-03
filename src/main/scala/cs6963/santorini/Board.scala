package cs6963.santorini
import io.circe.syntax._
import io.circe.{ Decoder, JsonObject, ObjectEncoder }
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

trait P {
  def card: String
}

@JsonCodec
case class PrePlayer(card: String) extends P

@JsonCodec
case class Player(tokens: List[List[Int]], card: String) extends P

@JsonCodec
case class Board(turn: Int, players: List[Player], spaces: List[List[Int]]) {

  //lazy val actions: List[Action] = validActions()

  def validActions(): Any = {
    val actions = players.head.card match {
      case "Nocard" => {

      }
      case "Hephastus" => {

      }
      case "Prometheus" => {

      }
    }
  }
}

object Board {
  def handleFirst(prePlayers: Any): String = {
    val(player1, player2) = prePlayers
    player2 match {
      case p: PrePlayer => println("1")
      case p: Player => println("2")
    }
    ""
  }
}
