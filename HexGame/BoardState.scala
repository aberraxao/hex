package HexGame

import HexGame.Board.{Board, getSize}
import Utils.RandomWithState

import scala.annotation.tailrec

case class BoardState(board: Board, boardSize: Int) {
  def this(boardSize: Int) {
    this(List.fill(boardSize, boardSize)(Cells.Empty), boardSize)
  }

  def this(board: Board) {
    this(board, getSize(board));
  }
}

object Board {
  type Board = List[List[Cells.Cell]]
  //type PlayerRandom = Player
  // type PlayerUser = Player

  def getSize(board: Board): Int = {
    board.length
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
  def play(board: Board, player: Cells.Cell, position: (Int, Int)): Board = {
    if (!isValidPlay(board, position))
      board
    else {
      println("Player " + player + ": " + position)
      board.updated(position._1, board(position._1).updated(position._2, player))
    }
  }

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

  def printBoardInner(board: Board, boardSize: Int): Unit = {

    @tailrec
    def printBoardHeader(n: Int): Unit = {
      if (n > 0) {
        if (n == 1)
          print("\u001B[34m *\u001B[0m")
        else
          print("\u001B[34m *  \u001B[0m")
        printBoardHeader(n - 1)
      }
    }

    @tailrec
    def printBoardEdge(n: Int, boardRow: Int): Unit = {
      if (n > 0) {
        if (boardRow % 2 == 0 && n == 2)
          print("\u001B[31m*\u001B[0m")
        else
          print(" ")
        printBoardEdge(n - 1, boardRow)
      }
    }

    @tailrec
    def printBoardLine(n: Int): Unit = {
      if (n > 0) {
        if (n == 1)
          print("\\")
        else
          print("\\ / ")
        printBoardLine(n - 1)
      }
    }

    @tailrec
    def printRow(row: List[Cells.Cell], boardRow: Int): Unit = {
      if (row.nonEmpty) {
        val cellValue = row.head match {
          case Cells.Red => "\u001B[31mX\u001B[0m"
          case Cells.Blue => "\u001B[34mO\u001B[0m"
          case Cells.Empty => "."
        }

        if (row.tail.nonEmpty)
          print(cellValue + " - ")
        else
          print(cellValue + "\u001B[31m *\u001B[0m")

        printRow(row.tail, boardRow)
      }
      else println()
    }

    val boardRow = 2 * (boardSize - getSize(board))
    val boardEdgeVal = 2 + boardRow

    if (board.nonEmpty) {
      if (boardRow == 0) {
        printBoardHeader(boardSize)
        println()
      }

      printBoardEdge(boardEdgeVal, boardRow)
      printRow(board.head, boardRow)

      if (board.tail.nonEmpty) {
        printBoardEdge(boardEdgeVal + 1, boardRow + 1)
        printBoardLine(boardSize)
        println()
      }
      else {
        printBoardEdge(boardEdgeVal, 1)
        printBoardHeader(boardSize)
        println()
      }

      printBoardInner(board.tail, boardSize)
    }
  }
}