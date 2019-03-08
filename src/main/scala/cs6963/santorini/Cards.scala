package cs6963.santorini

import scala.util.Random.shuffle


case class Change(tokens: List[List[Int]], spaces: List[List[Int]]) {
  def add(ch2: Change): Change = {
    Change(
      this.tokens.flatten.zipAll(ch2.tokens.flatten,0,0).map { case (a, b) => a + b} grouped 2 toList,
      this.spaces.flatten.zipAll(ch2.spaces.flatten,0,0).map { case (a, b) => a + b} grouped 5 toList
    )
  }

  def addDirs(t: List[List[Int]]): Change = {
    Change(
      this.tokens.flatten.zipAll(t.flatten,0,0).map { case (a, b) => a + b} grouped 2 toList,
      this.spaces
    )
  }

  def buildOn(b: List[Int]): Change = {
    var tmp = this.spaces.flatten.toArray
    //println(b, tmp.mkString(" "))
    tmp(b(1) + 5 * b(0) - 6) += 1
    Change(
      this.tokens,
      tmp.toList.grouped(5).toList
    )
  }

  def buildToFour(b: List[Int], board:Board): Change = {
    var height = Board.getHeight(board, b)
    var tmp = this.spaces.flatten.toArray
    tmp(b(1) + 5 * b(0) - 6) += (4 - height)
    Change(
      this.tokens,
      tmp.toList.grouped(5).toList
    )
  }

}

object Cards {
  def TurnCheck(board1: Board, board2: Board): Boolean = {
    val token0 = board1.players(0).tokens(0)
    val token1 = board1.players(0).tokens(1)
    if ((actions(token0, board1) ++ actions(token1, board1)).contains(board2)) true
    else false
  }

  def PlayRandom(board: Board): Board = {
    val token0 = board.players(0).tokens(0)
    val token1 = board.players(0).tokens(1)
    shuffle(actions(token0, board) ++ actions(token1, board)).head
  }

  def listAll(board: Board): List[Board] = {
    val token0 = board.players(0).tokens(0)
    val token1 = board.players(0).tokens(1)
    actions(token0, board) ++ actions(token1, board)
  }

  def actions(token: List[Int], board: Board): List[Board] = {
    val init = List(Change(List(List(0,0),List(0,0),List(0,0),List(0,0)),
                      List(List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0))))

    val card = board.players(0).card
    val height = Board.getHeight(board, token)
    val token1: List[Int] = if (token == board.players(0).tokens(0)) board.players(0).tokens(1)
    else board.players(0).tokens(0)
    val isFirstToken: Boolean = if (token == board.players(0).tokens(0)) true else false

    val token2: List[Int] = board.players(1).tokens(0)
    val token3: List[Int] = board.players(1).tokens(1)

    def getActableDirs(pos: List[Int]): List[List[Int]] = {
      val (x, y) = (pos(0), pos(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
                      List(1, 1), List(1, 0), List(1, -1), List(0, -1))

      dirs.filter( d => {
        val (dx, dy) = (d(0)+x, d(1)+y)
        !(dx < 1 ||
          dx > 5 ||
          dy < 1 ||
          dy > 5 ||
          Board.getHeight(board, List(dx, dy)) > height + 1 ||
          Board.getHeight(board, List(dx, dy)) == 4 ||
          List(dx, dy) == token1 ||
          List(dx, dy) == token2 ||
          List(dx, dy) == token3 )
      })
    }

    def getApolloDirs(pos: List[Int]): List[List[List[Int]]] = {
      val (x, y) = (pos(0), pos(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
        List(1, 1), List(1, 0), List(1, -1), List(0, -1))

      val tokens_dirs = dirs.filter( d => {
        val (dx, dy) = (d(0)+x, d(1)+y)
        !(dx < 1 ||
          dx > 5 ||
          dy < 1 ||
          dy > 5 ||
          Board.getHeight(board, List(dx, dy)) > height + 1 ||
          Board.getHeight(board, List(dx, dy)) == 4 ||
          List(dx, dy) == token1)
      })

      tokens_dirs.map( d => {
        val (dx, dy) = (d(0) + x, d(1) + y)
        if (List(dx, dy) == token2) List(d,List(0,0),d.map(-_),List(0,0))
        else if (List(dx, dy) == token3) List(d,List(0,0),List(0,0),d.map(-_))
        else List(d,List(0,0),List(0,0),List(0,0))
      })
    }

    def getBuildableDirs(pos: List[Int]): List[List[Int]] = {
      val (x, y) = (pos(0), pos(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
        List(1, 1), List(1, 0), List(1, -1), List(0, -1))

      dirs.filter( d => {
        val (dx, dy) = (d(0)+x, d(1)+y)
        !(dx < 1 ||
          dx > 5 ||
          dy < 1 ||
          dy > 5 ||
          // Board.getHeight(board, List(dx, dy)) > height + 1 ||
          Board.getHeight(board, List(dx, dy)) == 4 ||
          List(dx, dy) == token1 ||
          List(dx, dy) == token2 ||
          List(dx, dy) == token3 )
      })
    }

    def getBuildableDirsAtlas(pos: List[Int]): List[List[Int]] = {
      val (x, y) = (pos(0), pos(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
        List(1, 1), List(1, 0), List(1, -1), List(0, -1))

      dirs.filter( d => {
        val (dx, dy) = (d(0)+x, d(1)+y)
        !(dx < 1 ||
          dx > 5 ||
          dy < 1 ||
          dy > 5 ||
          // Board.getHeight(board, List(dx, dy)) > height + 1 ||
          Board.getHeight(board, List(dx, dy)) == 4 ||
          List(dx, dy) == token1 ||
          List(dx, dy) == token2 || // Need consider changes
          List(dx, dy) == token3 ) // Need consider changes
      })
    }

    def getDirs(pos: List[Int]): List[List[Int]] = {
      // TODO: Prometheus might need this
      val height = Board.getHeight(board, pos)
      val (x, y) = (pos(0), pos(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
        List(1, 1), List(1, 0), List(1, -1), List(0, -1))
      dirs.filter(d => !(d(0)+x < 1 || d(0)+x > 5 || d(1)+y < 1 || d(1)+y > 5))
    }

    /*def Move(changes: List[Change]): List[Change] = {
      changes.flatMap(change =>
        getActableDirs(token.zip(change.tokens(0)).map {case (x,y) => x+y}).map(dir =>
          change.addDirs(List(dir,List(0,0),List(0,0),List(0,0)))
        )
      )
    }*/

    def Move(changes: List[Change]): List[Change] = {
      changes.flatMap(change => {
        val spaces = (board.spaces.flatten, change.spaces.flatten).zipped.map(_+_).grouped(5).toList

        getActableDirs((token, change.tokens(0)).zipped.map(_ + _)).map(dir =>
          change.addDirs(List(dir, List(0, 0), List(0, 0), List(0, 0)))
        )
      })
    }

    def Build(changes: List[Change]): List[Change] = {
      changes.flatMap(change =>
        getBuildableDirs((token, change.tokens(0)).zipped.map(_+_)).map( dir =>
          change.buildOn((token, change.tokens(0)).zipped.map(_+_).zip(dir).map {case (x,y) => x+y})
        )
      )
    }

    def toBoard(changes: List[Change]): List[Board] = {
      changes.map(change => {
        val tokens = change.tokens
        if (isFirstToken) Board.switchPlayerAndAddTurn(
          Board(
            board.turn,
            List(Player(List(
              token.zip(change.tokens(0)).map{case(x,y) => x+y},
              token1.zip(change.tokens(1)).map{case(x,y) => x+y}
            ), board.players(0).card),
              Player(List(
                token2.zip(change.tokens(2)).map{case(x,y) => x+y},
                token3.zip(change.tokens(3)).map{case(x,y) => x+y}
              ), board.players(1).card)
            ),
            board.spaces.flatten.zip(change.spaces.flatten).map{case(x,y) => x+y}.grouped(5).toList)
        )
        else Board.switchPlayerAndAddTurn(
          Board(
            board.turn,
            List(Player(List(
              token1.zip(change.tokens(1)).map{case(x,y) => x+y},
              token.zip(change.tokens(0)).map{case(x,y) => x+y}
            ), board.players(0).card),
              Player(List(
                token2.zip(change.tokens(2)).map{case(x,y) => x+y},
                token3.zip(change.tokens(3)).map{case(x,y) => x+y}
              ), board.players(1).card)
            ),
            board.spaces.flatten.zip(change.spaces.flatten).map{case(x,y) => x+y}.grouped(5).toList)
        )
        }
      )
    }

    def ApolloMove(changes: List[Change]): List[Change] = {
      changes.flatMap(change =>
        getApolloDirs(token.zip(change.tokens(0)).map {case (x,y) => x+y}).map(dir => {
          println(dir)
          change.addDirs(dir)
        })
      )
    }

    def ArtemisMove(changes: List[Change]): List[Change] = {
      changes ++ Move(changes)
    }

    def AtlasBuild(changes: List[Change]): List[Change] = {
      Build(changes) ++
        changes.flatMap(change =>
          getBuildableDirs(token.zip(change.tokens(0)).map {case (x,y) => x+y}).map( dir =>
            change.buildToFour(token.zip(change.tokens(0)).map {case (x,y) => x+y}.zip(dir).map {case (x,y) => x+y}, board)
          )
        )
    }

    def DemeterBuild(changes: List[Change]) {}//: List[Change] = {...}

    def HephastusBuild(changes: List[Change]): List[Change] = {
      //changes ++
      changes
    }

    def MinotaurMove(changes: List[Change]) {}//: List[Change] = {...}

    def PrometheusBuild(changes: List[Change]) {}//: List[Change] = {...}

    def PrometheusMove(changes: List[Change]) {}//: List[Change] = {...}

    toBoard(card match {
      case "Nocard" => Build( Move(init))
      case "Apollo" => Build( ApolloMove(init))
      case "Artemis" => Build( ArtemisMove( Move(init)))
      case "Atlas" => AtlasBuild( Move(init))
      //case "Demeter" => DemeterBuild( Build( Move(init)))
      //case "Hephastus" => HephastusBuild( Build( Move(init)))
      //case "Minotaur" => Build( MinotaurMove(init))
      //case "Prometheus" => Build( PrometheusMove( PrometheusBuild(init)))
    })
  }
}

object run extends App {
  val board = JSON.parseJSON("""{"turn":12,"players":[[[3,2],[5,1]],[[2,4],[4,3]]],"spaces":[[0,1,0,0,0],[1,1,2,0,0],[0,1,0,1,1],[0,0,1,1,0],[0,1,1,0,0]]}""")
  // token 0
  println(board)
  println(Cards.PlayRandom(board))

  val board1 = JSON.parseJSON("""{"turn":0,"players":[{"tokens":[[3,5],[4,3]],"card":"Apollo"},{"tokens":[[1,4],[4,4]],"card":"Artemis"}],"spaces":[[0,0,0,0,0],[0,0,0,2,2],[0,4,4,4,0],[0,4,1,1,4],[1,4,4,4,4]]}""")
  val board2 = JSON.parseJSON("""{"turn":1,"players":[{"tokens":[[1,4],[4,3]],"card":"Artemis"},{"tokens":[[3,5],[4,4]],"card":"Apollo"}],"spaces":[[0,0,0,0,0],[0,0,0,2,2],[0,4,4,4,0],[0,4,2,1,4],[1,4,4,4,4]]}""")

  println(Cards.listAll(board1).map(x => JSON.encode(x)).mkString("\n"))


}
