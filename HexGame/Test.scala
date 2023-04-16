package HexGame

import HexGame.Board.{Board, getSize, play, printBoard, randomMove, undo, undoSinglePlay}
import Utils.{MyRandom, RandomWithState}

object Test extends App {

  val r = MyRandom(10)
  val test = new BoardState(5)
  printBoard(test)

  /*
  println("Player red: 1 1")
  val newBoard = play(test.board, Cells.Red, 1, 1)
  val newTest = new BoardState(newBoard)
  printBoard(newTest.board, getSize(test.board))
   */

  val newTest = new BoardState(play(test.board, Cells.Blue, 2, 2))
  println("Player blue: 2 2")
  printBoard(newTest)

  val newTest2 = new BoardState(play(newTest.board, Cells.Red, 2, 1))
  println("Player red: 2 1")
  printBoard(newTest2)

  val newRand = randomMove(newTest2.board, r)
  val newPos = newRand._1
  val newTest3 = new BoardState(play(newTest2.board, Cells.Blue, newPos))
  println("Player blue pc: " + newPos)
  printBoard(newTest3)

  val newRand1 = randomMove(newTest2.board, newRand._2)
  val newPos2 = randomMove(newTest2.board, newRand1._2)._1
  val newTest4 = new BoardState(play(newTest3.board, Cells.Red, newPos2))
  println("Player red pc: " + newPos2)
  printBoard(newTest4)

  val newTest5 = new BoardState(undoSinglePlay(newTest4.board, newPos2))
  println("Undo: " + newPos2)
  printBoard(newTest5)

  /*
  println("Player red: 1 2")
  test.board = play(test.board, Cells.Red, 1, 2)
  printBoard(test.board, getSize(test.board))
  */
}