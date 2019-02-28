package cs6963.santorini
import io.circe.syntax._
import io.circe.{ Decoder, JsonObject, ObjectEncoder }
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }


@JsonCodec
case class Player(tokens: List[List[Int]], card: String)

@JsonCodec
case class Board(turn: Int, players: List[Player], spaces: List[List[Int]])




/*object tmp extends App {
  val board = JSON.parseJSON("""{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}""")
  print(board.asJson.noSpaces)

}*/