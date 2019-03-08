package cs6963.santorini

case class Change(tokens: List[List[Int]], spaces: List[List[Int]])

object Cards {
  def actions(card: String, token: List[Int], board: Board): List[Change] = { // TODO: Will be :List[Build] when finish
    val init = List(Change(List(List(0,0),List(0,0),List(0,0),List(0,0)),
                      List(List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0))))

    def getMoveableDirs(pos: List[Int]): List[List[Int]] = {
      val height = Board.getHeight(board, pos)
      val (x, y) = (pos(0), pos(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
                      List(1, 1), List(1, 0), List(1, -1), List(0, -1))
      dirs.filter(d => !(d(0)+x < 1 || d(0)+x > 5 || d(1)+y < 1 || d(1)+y > 5 ||
                         Board.getHeight(board, List(d(0)+x, d(1)+y)) > height + 1 ||
                         Board.getHeight(board, List(d(0)+x, d(1)+y)) == 4 ))
    }

    def getDirs(pos: List[Int]): List[List[Int]] = {
      // TODO: Prometheus might need this
      val height = Board.getHeight(board, pos)
      val (x, y) = (pos(0), pos(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
        List(1, 1), List(1, 0), List(1, -1), List(0, -1))
      dirs.filter(d => !(d(0)+x < 1 || d(0)+x > 5 || d(1)+y < 1 || d(1)+y > 5)
    }

    def Move(changes: List[Change]): List[Change] = {
     

      val dirs = getMoveableDirs(token)
      println(dirs)
      List(Change(List(List(1,2)), List(List(1,2))))
    }

    def Build(changes: List[Change]): List[Change] = {
      List(Change(List(List(1,2)), List(List(1,2))))
    }

    def ApolloMove(changes: List[Change]) {}//: List[Change] = {...}

    def ArtemisMove(changes: List[Change]) {}//: List[Change] = {...}

    def AtlasBuild(changes: List[Change]) {}//: List[Change] = {...}

    def DemeterBuild(changes: List[Change]) {}//: List[Change] = {...}

    def HephastusBuild(changes: List[Change]) {}//: List[Change] = {...}

    def MinotaurMove(changes: List[Change]) {}//: List[Change] = {...}

    def PrometheusBuild(changes: List[Change]) {}//: List[Change] = {...}

    def PrometheusMove(changes: List[Change]) {}//: List[Change] = {...}

    card match {
      case "Nocard" => Build( Move(init))
      //case "Apollo" => Build( ApolloMove(init))
      //case "Artemis" => Build( ArtemisMove( Move(init)))
      //case "Atlas" => AtlasBuild( Move(init))
      //case "Demeter" => DemeterBuild( Build( Move(init)))
      //case "Hephastus" => HephastusBuild( Build( Move(init)))
      //case "Minotaur" => Build( MinotaurMove(init))
      //case "Prometheus" => Build( PrometheusMove( PrometheusBuild(init)))
    }
  }
}

object run extends App {
  val board = JSON.parseJSON("""{"turn":12,"players":[[[3,2],[5,1]],[[2,4],[4,3]]],"spaces":[[0,1,0,0,0],[1,1,2,0,0],[0,1,0,1,1],[0,0,1,1,0],[0,1,1,0,0]]}""")
  // token 0
  val token0 = List(board.players(0).tokens(0)(0), board.players(0).tokens(0)(1))
  // token 1
  val token1 = List(board.players(0).tokens(1)(0), board.players(0).tokens(1)(1))
  println(board, token0)
  println(Cards.actions("Nocard", token0, board))
  println(Cards.actions("Nocard", token1, board))
}