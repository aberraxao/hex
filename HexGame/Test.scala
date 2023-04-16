package HexGame

import HexGame.Board.{getSize, play, printBoard}

object Test extends App {

  val test = new BoardState(5)
  printBoard(test.board, Board.getSize(test.board))

  /*
  println("Player red: 1 1")
  val newBoard = play(test.board, Cells.Red, 1, 1)
  val newTest = new BoardState(newBoard)
  printBoard(newTest.board, getSize(test.board))
   */

  println("Player blue: 2 2")
  val newTest = new BoardState(play(test.board, Cells.Blue, 2, 2))
  printBoard(newTest.board, getSize(test.board))

  println("Player red: 2 2")
  val newTest2 = new BoardState(play(newTest.board, Cells.Red, 2, 1))
  printBoard(newTest2.board, getSize(test.board))

  /*
  println("Player red: 1 2")
  test.board = play(test.board, Cells.Red, 1, 2)
  printBoard(test.board, getSize(test.board))
  */
}