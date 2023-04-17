package HexGame

import HexGame.Board.{Board, getSize}
import Utils.RandomWithState

import scala.annotation.tailrec

case class BoardState(board: Board, boardSize: Int) {
  def this(boardSize: Int) {
    this(List.fill(boardSize, boardSize)(Cells.Empty), boardSize)
  }

  def this(board: Board) {
    this(board, getSize(board))
  }
}

object Board {
  type Board = List[List[Cells.Cell]]
  // type PlayerRandom = Player
  // type PlayerUser = Player

  def getSize(board: Board): Int = {
    board.length
  }

  def play(board: Board, player: Cells.Cell, position: (Int, Int)): Board = {
    if (!isValidPlay(board, position))
      board
    else {
      println("Player " + player + ": " + position)
      board.updated(position._1, board(position._1).updated(position._2, player))
    }
  }

  /**
   * Função que efetua uma jogada no tabuleiro numa determinada posição.
   * Verifica se a posição está livre e efetua a jogada.
   * Caso contrário, devovle o tabuleiro sem alterações.
   *
   * @param board  tabuleiro de jogo no qual se vai efetuar a jogada
   * @param player jogador que está a efetuar a jogada
   * @param row    número da linha onde vai ser efetuada a jogada
   * @param col    número da coluna onde vai ser efetuada a jogada
   * @return tabuleiro de jogo atualizado (com ou sem jogada efetuada)
   */
  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = {
    play(board, player, (row, col))
  }

  def undo(board: Board, positions: ((Int, Int), (Int, Int))): Board = {
    if (positions == null)
      board
    else {
      println("Undo was done")
      undoSinglePlay(undoSinglePlay(board, positions._1), positions._2)
    }
  }

  def undoSinglePlay(board: Board, position: (Int, Int)): Board = {
    if (!isValidUndo(board, position))
      board
    else
      board.updated(position._1, board(position._1).updated(position._2, Cells.Empty))
  }

  def getPlay(board: Board, player: String, position: (Int, Int)): Board = {
    if (!isValidPlay(board, position))
      board
    else {
      player match {
        case "Red" => board.updated(position._1, board(position._1).updated(position._2, Cells.Red))
        case "Blue" => board.updated(position._1, board(position._1).updated(position._2, Cells.Blue))
        case "Empty" => board
        case _ => println("Há um erro no ficheiro"); board
      }
    }
  }

  def isPositionInsideBoard(board: Board, position: (Int, Int)): Boolean = {
    if (position == null ||
      position._1 < 0 ||
      position._1 >= getSize(board) ||
      position._2 < 0 ||
      position._2 >= getSize(board))
      false
    else
      true
  }

  def isPositionEmpty(board: Board, position: (Int, Int)): Boolean = {
    if (board(position._1)(position._2) == Cells.Empty)
      true
    else
      false
  }

  def isValidPlay(board: Board, position: (Int, Int)): Boolean = {
    if (isPositionInsideBoard(board, position) && isPositionEmpty(board, position))
      true
    else
      false
  }

  def isValidUndo(board: Board, position: (Int, Int)): Boolean = {
    if (position != null && isPositionInsideBoard(board, position) && !isPositionEmpty(board, position))
      true
    else
      false
  }

  @tailrec
  def randomMove(board: Board, rand: RandomWithState): ((Int, Int), RandomWithState) = {
    val (row, firstRandom) = rand.nextInt(board.length)
    val (col, nextRandom) = firstRandom.nextInt(board.length)
    if (board(row)(col) == Cells.Empty)
      ((row, col), nextRandom)
    else
      randomMove(board, nextRandom)
  }

  /**
   * Função para representar o tabuleiro visualmente com slashes e backslashes
   */
  def printBoard(boardState: BoardState): Unit = {
    val boardSize = boardState.boardSize
    printBoardInner(boardState.board, boardSize)
  }


  def printBoardInner(board: Board, size: Int): Unit = {
    val size = board.length

    def printSpace(n: Int): Unit = {
      if (n > 0) {
        print(" ")
        printSpace(n - 1)
      }
    }

    def cellVal(cell: Cells.Cell): String = cell match {
      case Cells.Red => "\u001B[31mX\u001B[0m"
      case Cells.Blue => "\u001B[34mO\u001B[0m"
      case Cells.Empty => "."
    }

    @tailrec
    def printBoardHeader(n: Int): Unit = {
      if (n > 0) {
        if (n == 1)
          print("\u001B[34m *\n\u001B[0m")
        else
          print("\u001B[34m *  \u001B[0m")
        printBoardHeader(n - 1)
      }
    }

    @tailrec
    def printRow(row: List[Cells.Cell]): Unit = row match {
      case x :: Nil => print(cellVal(x) + "\u001B[31m *\u001B[0m\n")
      case x :: xs => print(cellVal(x) + " - "); printRow(xs)
    }

    def printRow2(row: List[Cells.Cell]): Unit = row match {
      case x :: Nil => print(" \\ \n")
      case x :: xs => print(" \\ /"); printRow2(xs)
    }

    @tailrec
    def aux(board2: Board, acc: Int): Unit = {
      board2 match {
        case x :: Nil =>
          printSpace(acc);
          print("\u001B[31m* \u001B[0m");
          printRow(x)
          printSpace(acc + 2);
          printBoardHeader(size)
        case x :: xs =>
          printSpace(acc);
          print("\u001B[31m* \u001B[0m");
          printRow(x)
          printSpace(acc + 2);
          printRow2(x)
          aux(board2.tail, acc + 2)
      }
    }

    printBoardHeader(size)
    aux(board, 0)
  }

  def saveBoard(boardState: BoardState, fileName: String): Unit = {
    Utils.File_Utils.saveCsv(fileName, boardState)
  }

  def readBoard(fileName:String): BoardState = {
    Utils.File_Utils.readCsv(fileName)
  }

}