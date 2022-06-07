import Main.play

import scala.Console.in
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.control.Breaks.break

object Main{
  type Line = List[Player]
  type Board = List[Line]

  // creates a board from a string
  def makeBoard(s: String): Board = {
    def toPos(c: Char): Player = {
      c match {
        case 'X' => One
        case '0' => Two
        case _ => Empty
      }
    }
    s.split("\n").toList.foldLeft(List[Line]()){ (acc, currentString) =>
      acc :+ currentString.map(x => toPos(x)).toList
    }
  }

  // checks if the position (x,y) board b is free
  def isFree(x:Int, y:Int, b:Board):Boolean = {
    if(b(x)(y) != Empty) false
    else true
  }

  // returns the "other" player from a position, if it exists
  def complement(p: Player): Player = {
    if(p == One) Two
    else if(p == Two) One
    else Empty
  }

  def show(b: Board): String = {
    def toStringPos(p: Player): Char = {
      p match {
        case One => 'X'
        case Two => '0'
        case Empty => '.'
      }
    }
    b.map(x => x.map(l => toStringPos(l))).flatMap(x => x :+ "\n").mkString.dropRight(1)
  }

  // Returns a list of columns from a board
  def getColumns(b: Board): Board = {
    for (i <- b.indices.toList) yield
      for (j <- b(i).indices.toList) yield
        b(j)(i)
  }

  //returns the first diagonal as a line
  def getFstDiag(b:Board): Line = {
    for (i <- b.indices.toList) yield
      b(i)(i)
  }

  //returns the second diagonal as a line
  def getSndDiag(b:Board): Line = {
    for (i <- b.indices.toList) yield
      b(i)(b.size - i - 1)
  }

  // retrieves all the diagonals above the first line
  def getAboveFstDiag(b: Board): List[Line] = {
    val aboveFstDiag = {
      for (i <- b.indices.toList) yield {
        for (j <- b.indices.toList if i < j) yield {
          b(i)(j)
        }
      }
    }
    getColumns(aboveFstDiag).dropRight(1)
  }

  def getBelowFstDiag(b: Board): List[Line] = {
    getAboveFstDiag(getColumns(b))
  }

  def getAboveSndDiag(b: Board): List[Line] = {
    getBelowFstDiag(getColumns(b).reverse)
  }

  def getBelowSndDiag(b: Board): List[Line] = {
    getAboveFstDiag(getColumns(b).reverse)
  }

  //write a function which checks if a given player is a winner
  def winner(p: Player)(b: Board): Boolean = {
    def checkWinner(b: Board, size: Int): Boolean = {
      if(!b.forall(x => {
        var winningScore = 0
        for (i <- x.indices) {
          if (x(i) == p)
            winningScore += 1
        }
        if (winningScore == size) {
          false
        } else true
      })) true else false
    }

    val diagBoard: Board = List(getFstDiag(b), getSndDiag(b))

    if(checkWinner(b, b.size))
      true
    else if(checkWinner(getColumns(b), b.size))
      true
    else if(checkWinner(diagBoard, b.size))
      true
    else
      false
  }

  def update(p: Player)(ln: Int, col: Int, b: Board) : Board = {
    for (i <- b.indices.toList) yield
      for (j <- b.indices.toList) yield
        if (i == ln && j == col)
          p
        else
          b(i)(j)
  }

  /*
   * generates one possible next move for player p.
   */
  def next(p: Player)(b: Board): List[Board] = {
    val indices = {
      for (i <- b.indices) yield
        for (j <- b(i).indices) yield
          if (isFree(i, j, b))
            (i, j)
          else
            0
    }
    val boardsList = {
      indices.flatten.filter(x => x != 0).toList.map {
        case (i, j) => b.foldLeft(List[Board]()) { (acc, _) =>
          update(p)(i.toString.toInt, j.toString.toInt, b) :: acc
        }
      }
    }
    boardsList.flatten
  }

  def sequences(p: Player)(b: Board): Map[Int, Int] = {
    def checkSequences(b: Board, seqNo: Int): Int = {
      var sequences = 0
      b.foreach(x => {
        var winningScore = 0
        for (i <- x.indices) {
          if (x(i) == p)
            winningScore += 1
          if (x(i) == complement(p))
            winningScore -= 2000
        }

        if (winningScore == seqNo)
          sequences += 1
      })
      sequences
    }
    def checkNoSequences(b: Board, seqNo: Int): Int = {
      val diagBoard: Board = List(getFstDiag(b), getSndDiag(b))
      checkSequences(b, seqNo) + checkSequences(diagBoard, seqNo) + checkSequences(getColumns(b), seqNo)
    }
    Map(5 -> checkNoSequences(b, 5), 4 -> checkNoSequences(b, 4),
        3 -> checkNoSequences(b, 3), 2 -> checkNoSequences(b, 2),
        1 -> checkNoSequences(b, 1))
  }

  def AI(b: Board, p: Player): Board = {
    def calculateScore(b: Board): Int = {
      def calculateMyScore(b: Board): Int = {
        for ((k, v) <- sequences(p)(b).toList) yield {
          if (k == b.size)
            +1000 * v
          else
            +k * v
        }
      }.sum
      def calculateEnemyScore(b: Board): Int = {
        for ((k, v) <- sequences(complement(p))(b).toList) yield {
          if (k == b.size)
            +2000 * v
          else
            +k * v
        }
      }.sum
      calculateMyScore(b) - calculateEnemyScore(b)
    }
    var board = b
    var max = -3000
    next(p)(b).foreach(x => {
      if(max < calculateScore(x)) {
        max = calculateScore(x)
        board = x
      }
    })
    board
  }

  def play(b: Board, p: Player): Unit = {
    @tailrec
    def game(turn: Int, acc: Board): Unit = {
      val player = if (turn % 2 == 0) One else Two
      val message = s"Player $player Turn!"
      val board =
        if (p == player) {
          println(message + " Insert coordinates: ")
          val oldAcc = acc
          var updatedBoard = oldAcc
          while (updatedBoard == oldAcc) {
            try {
              val Array(x, y) = in.readLine().split(" ").map(_.toInt)
              if (isFree(x - 1, y - 1, updatedBoard)) {
                updatedBoard = update(player)(x - 1, y - 1, updatedBoard)
              } else {
                println("Position already occupied.")
              }
            } catch {
              case e: Exception => println("Please enter valid coordinates.")
            }
          }
          updatedBoard
        } else {
          AI(acc, player)
        }

      println(show(board))
      if (winner(One)(board)) {
        println("Player One won!")
      } else if (winner(Two)(board)) {
        println("Player Two won!")
      } else {
        game(turn + 1, board)
      }
    }

    game(0, b)
  }

  // for testing purposes only.
  def main(args: Array[String]): Unit = {

    val emptyBoardString =
      """.....
        |.....
        |.....
        |.....
        |.....""".stripMargin
    val emptyBoard = makeBoard(emptyBoardString)
    println("Game has started!")
    println("Press 1 to play as Player One vs AI, 2 as Player Two vs AI, anything else to AI vs AI")
    val decision = in.readLine().toInt
    if(decision == 1) {
      println(show(emptyBoard))
      play(emptyBoard, One)
    } else if(decision == 2) {
      play(emptyBoard, Two)
    } else
      play(emptyBoard, Empty)
    println("Game over")
  }
}

