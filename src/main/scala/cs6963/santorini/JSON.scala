package cs6963.santorini
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.JsonCodec

//case class Player(tokens: List[List[Int]], card: String)
//case class Spaces(spaces: List[List[Int]])

object JSON {
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

    if (input contains "tokens") {
      val playersEither = cursor.downField("players").as[List[Player]]
      val players = playersEither match {
        case Right(ps) => ps
        case Left(error) => println(error.getMessage())
      }
      Board(turn.toString.toInt, players.asInstanceOf[List[Player]], spaces.asInstanceOf[List[List[Int]]])
    } else {
      val playersEither = cursor.downField("players").as[List[List[List[Int]]]]
      val players = playersEither match {
        case Right(ps) => List(Player(ps(0), "Nocard"), Player(ps(1), "Nocard"))
        case Left(error) => println(error.getMessage())
      }
      Board(turn.toString.toInt, players.asInstanceOf[List[Player]], spaces.asInstanceOf[List[List[Int]]])
    }
  }

  def parsePre (input: String)= {
    val json: Json = parser.parse(input).getOrElse(Json.Null)
    val cursor: HCursor = json.hcursor
    val prePlayerEither = cursor.as[(PrePlayer, Player)]
    val prePlayers = prePlayerEither match {
      case Right(s) => s
      case Left(error) => {
        val prePlayerEither2 = cursor.as[(PrePlayer, PrePlayer)]
        val prePlayers2 = prePlayerEither2 match {
          case Right(s) => s
          case Left(error2) => {
            System.err.println("Playing with no card...")
            val noCardPlayerEither = cursor.as[List[List[List[Int]]]]
            val noCardPlayer = noCardPlayerEither match {
              case Right(s) => {
                s match {
                  case List() => (PrePlayer("Nocard"), PrePlayer("Nocard"))
                  case List(xs) => (PrePlayer("Nocard"), Player(xs, "Nocard"))
                }
              }
              case Left(error3) => {
                println("Error: Invalid input.")
              }
            }
            noCardPlayer
          }
        }
        prePlayers2
      }
    }
    prePlayers
  }

  @JsonCodec
  case class NoCardBoard(turn: Int, players: List[List[List[Int]]], spaces: List[List[Int]])

  def encode (board: Board): String = {
    (board.players(0).card, board.players(1).card) match {
      case ("Nocard", "Nocard") => NoCardBoard(board.turn, List(board.players(0).tokens, board.players(1).tokens), board.spaces).asJson.noSpaces
      case (_, _) => board.asJson.noSpaces
    }
  }
  def encode (player1: PrePlayer, player2: Player): String = {
    (player1.card, player2.card) match {
      case ("Nocard", "Nocard") => List(player2.tokens).asJson.noSpaces
      case (_, _) => "[" + player1.asJson.noSpaces + "," + player2.asJson.noSpaces + "]"
    }
  }
  def encode (player1: Player, player2: Player): String = {
    (player1.card, player2.card) match {
      case ("Nocard", "Nocard") => List(player1.tokens, player2.tokens).asJson.noSpaces
      case (_, _) => "[" + player1.asJson.noSpaces + "," + player2.asJson.noSpaces + "]"
    }
  }

}

object tmp2 extends App {
  val json1 = JSON.parsePre("""[]""")
  val json2 = JSON.parsePre("""[[[1,5],[2,2]]]""")
  val json3 = JSON.parsePre("""[{"card":"Artemis"},{"card":"Prometheus"}]""")
  println(json1)
  println(json2)
  println(json3)
  println((PrePlayer("Nocard"), PrePlayer("Nocard")))
}