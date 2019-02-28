package cs6963.santorini
import io.circe._
import io.circe.generic.auto._

case class Player(tokens: List[List[Int]], card: String)
//case class Spaces(spaces: List[List[Int]])

object JSON{
  def parseJSON (input: String): Board = {

    val json: Json = parser.parse(input).getOrElse(Json.Null)
    val cursor: HCursor = json.hcursor
    val spacesEither = cursor.downField("spaces").as[List[List[Int]]]
    val spaces = spacesEither match {
      case Right(s) => s
      case Left(error) => println(error.getMessage())
    }

    val turnEither = cursor.downField("turn").as[Int]
    val turn = turnEither match {
      case Right(i) => i
      case Left(error) => println(error.getMessage())
    }

    val playersEither = cursor.downField("players").as[List[Player]]
    val players = playersEither match {
      case Right(ps) => ps
      case Left(error) => println(error.getMessage())
    }

    //println(players)
    //println(turn, spaces)
    Board(turn.toString.toInt, players.asInstanceOf[List[Player]](0), players.asInstanceOf[List[Player]](1), spaces.asInstanceOf[List[List[Int]]])
  }

}
