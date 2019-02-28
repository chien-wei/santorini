package cs6963.santorini
import io.circe._
import io.circe.generic.auto._

case class Player(tokens: List[List[Int]], card: String)

object JSON {
  def main (args: Array[String] ): Unit = {
    val our = """{"turn":18,"players":[{"tokens":[[2,3],[4,4]],"card":"Artemis"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"""

    val json: Json = parser.parse(our).getOrElse(Json.Null)
    val cursor: HCursor = json.hcursor
    val spacesEither = cursor.downField("spaces").as[List[List[Int]]]
    val spaces = spacesEither match {
      case Right(s) => s
      case Left(error) => println(error.getMessage())
    }

    val turnEither = cursor.downField("turn").as[Int]
    val turn = spacesEither match {
      case Right(i) => i
      case Left(error) => println(error.getMessage())
    }

    val playersEither = cursor.downField("players").as[List[Player]]
    val players = playersEither match {
      case Right(ps) => ps
      case Left(error) => println(error.getMessage())
    }
    println(players)
    println(turn, spaces)
  }
}
