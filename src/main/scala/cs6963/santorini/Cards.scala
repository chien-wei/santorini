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

  def addSpaces(s: List[List[Int]]): Change = {
    Change(
      this.tokens,
      this.spaces.flatten.zipAll(s.flatten,0,0).map { case (a, b) => a + b} grouped 5 toList
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
    if ((actions(true, board1) ++ actions(false, board1)).contains(board2)) true
    else false
  }

  def PlayRandom(board: Board): Board = {
    shuffle(actions(true, board) ++ actions(false, board)).head
  }

  def listAll(board: Board): List[Board] = {
    actions(true, board) ++ actions(false, board)
  }

  def actions(isFirstToken: Boolean, board: Board): List[Board] = {
    val init = List(Change(List(List(0,0),List(0,0),List(0,0),List(0,0)),
                      List(List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0),List(0,0,0,0,0))))

    val token = if (isFirstToken) board.token0 else board.token1
    val card = board.players(0).card
    val height = Board.getHeight(board, token)

    def getMovableDirs(isFirstToken: Boolean, board: Board): List[List[List[Int]]] = {
      val (x, y) = if (isFirstToken) (board.token0(0), board.token0(1)) else (board.token1(0), board.token1(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
                      List(1, 1), List(1, 0), List(1, -1), List(0, -1))

      val result = dirs.filter( d => {
        val (dx, dy) = (d(0)+x, d(1)+y)
        val dxy = List(dx, dy)
        !(dx < 1 ||
          dx > 5 ||
          dy < 1 ||
          dy > 5 ||
          Board.getHeight(board, dxy) > height + 1 ||
          Board.getHeight(board, dxy) == 4 ||
          dxy == board.token0 ||
          dxy == board.token1 ||
          dxy == board.token2 ||
          dxy == board.token3 )
      })
      result.map(res => if (isFirstToken) List(res, List(0,0), List(0,0), List(0,0))
      else List(List(0,0), res, List(0,0), List(0,0)))
    }

    def getApolloMoveDirs(pos: Boolean, board: Board): List[List[List[Int]]] = {
      val (x, y) = if (isFirstToken) (board.token0(0), board.token0(1)) else (board.token1(0), board.token1(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
        List(1, 1), List(1, 0), List(1, -1), List(0, -1))

      val result = dirs.filter( d => {
        val (dx, dy) = (d(0)+x, d(1)+y)
        val dxy = List(dx, dy)
        !(dx < 1 ||
          dx > 5 ||
          dy < 1 ||
          dy > 5 ||
          Board.getHeight(board, dxy) > height + 1 ||
          Board.getHeight(board, dxy) == 4 ||
          dxy == board.token0 ||
          dxy == board.token1)
      })

      result.map(res =>
      {
        if (isFirstToken) {
        if ((res,board.token0).zipped.map(_+_) == board.token2) List(res, List(0,0), res.map(-_), List(0,0))
        else if ((res,board.token0).zipped.map(_+_) == board.token3) List(res, List(0,0), List(0,0), res.map(-_))
        else List(res, List(0,0), List(0,0), List(0,0))
      }
      else {
        if ((res,board.token1).zipped.map(_+_) == board.token2) List(List(0,0), res, res.map(-_), List(0,0))
        else if ((res,board.token1).zipped.map(_+_) == board.token3) List(List(0,0), res, List(0,0), res.map(-_))
        else List(List(0,0), res, List(0,0), List(0,0))
      }})
    }

    def getBuildableDirs(isFirstToken: Boolean, board: Board): List[List[List[Int]]] = {
      val (x, y) = if (isFirstToken) (board.token0(0), board.token0(1)) else (board.token1(0), board.token1(1))
      val dirs = List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, 1),
        List(1, 1), List(1, 0), List(1, -1), List(0, -1))

      val result = dirs.filter( d => {
        val (dx, dy) = (d(0)+x, d(1)+y)
        val dxy = List(dx, dy)
        !(dx < 1 ||
          dx > 5 ||
          dy < 1 ||
          dy > 5 ||
          Board.getHeight(board, dxy) >= 4 ||
          dxy == board.token0 ||
          dxy == board.token1 ||
          dxy == board.token2 ||
          dxy == board.token3 )
      })

      result.map(res => if (isFirstToken) {
        var spaces = List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0)).flatten.toArray
        val sxy = (res,board.token0).zipped.map(_+_)
        spaces(sxy(1) + 5 * sxy(0) - 6) += 1
        spaces.toList.grouped(5).toList
      }
      else {
        var spaces = List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0)).flatten.toArray
        val sxy = (res,board.token1).zipped.map(_+_)
        spaces(sxy(1) + 5 * sxy(0) - 6) += 1
        spaces.toList.grouped(5).toList
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

    def Move(changes: List[Change]): List[Change] = {
      changes.flatMap(change => {
        val newBoard = board.addChange(change)
        getMovableDirs(isFirstToken, newBoard).map(dirs =>
          change.addDirs(dirs)
        )
      })
    }

    def Build(changes: List[Change]): List[Change] = {
      changes.flatMap(change => {
        val newBoard = board.addChange(change)
        getBuildableDirs(isFirstToken, newBoard).map(spaces =>
          change.addSpaces(spaces)
        )
      })
    }

    def toBoard(changes: List[Change]): List[Board] = {
      changes.map(change => Board.switchPlayerAndAddTurn(board.addChange(change)))
    }

    def ApolloMove(changes: List[Change]): List[Change] = {
      changes.flatMap(change => {
        val newBoard = board.addChange(change)
        getApolloMoveDirs(isFirstToken, newBoard).map(dir => {
          change.addDirs(dir)
        })
      })
    }

    def ArtemisMove(changes: List[Change]): List[Change] = {
      changes ++ Move(changes)
    }

    def AtlasBuild(changes: List[Change]): List[Change] = {
      changes
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

  val board1 = JSON.parseJSON("""{"turn":0,"players":[{"tokens":[[2,3],[4,4]],"card":"Apollo"},{"tokens":[[2,5],[3,5]],"card":"Prometheus"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}""")
  val board2 = JSON.parseJSON("""{"turn":1,"players":[{"tokens":[[2,5],[4,4]],"card":"Prometheus"},{"tokens":[[2,3],[3,5]],"card":"Apollo"}],"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,1],[0,0,0,1,4]]}""")

  println(board1)
  println(Cards.listAll(board1).map(x => JSON.encode(x)).mkString("\n"))
  println()
  //println(Cards.listAll(board).map(x => JSON.encode(x)).mkString("\n"))


}