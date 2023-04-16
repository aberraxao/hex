package HexGame

import Utils.RandomWithState

import scala.annotation.tailrec

case class BoardState(boardSize: Int){
  val board: Board.Board = List.fill(boardSize, boardSize)(Cells.Empty)
}

/*
def emptyBoard(size: Int, count1: Int): Board = {
    def emptyList(count2: Int): List[Cells.Cell] = {
      if (count2 == 0)
        Nil
      else
        Cells.Empty :: emptyList(count2 - 1)
    }

    if(count1 == 0)
      Nil
    else
      emptyList(size) :: emptyBoard(size, count1 - 1)
  }
 */

object Board {
  type Board = List[List[Cells.Cell]]
  //type PlayerRandom = Player
 // type PlayerUser = Player

  def printBoard(board:Board): Unit = {
    println("Board: ")
    board map (x => print(x) + "\n")
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
  def play(board: Board, boardSize:Int, player: Cells.Cell, row: Int, col: Int): Board = {

    if (row < 0 || row >= boardSize || col < 0 || col >= boardSize || board(row)(col) != Cells.Empty) {
      board
    } else {
      board.updated(row, board(row).updated(col, player))
    }
  }
  /*
  def play(board: Board, player: Cells.Cell, row: Int, col: Int): Board = {
    def aux(b: Board, r: Int, c: Int): Board = b match {
      case Nil => Nil
      case x :: xs => if (r != row) x :: aux(xs, r + 1, col) else aux2(x, c) :: xs
    }

    def aux2(l: List[Cells.Cell], c: Int): List[Cells.Cell] = l match {
      case Nil => Nil
      case y :: ys => if (c == col) player :: ys else y :: aux2(ys, c + 1)
    }

    aux(board, 0, 0)
  }
*/


  @tailrec
  def randomMove(board: Board, rand: RandomWithState): ((Int, Int), RandomWithState) = {
    val (row, firstRandom) = rand.nextInt(board.length)
    val (col, nextRandom) = firstRandom.nextInt(board.length)
    if (board(row)(col) == Cells.Empty)
      ((row, col), nextRandom)
    else
      randomMove(board, nextRandom)
  }

}

