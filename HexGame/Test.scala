package HexGame

import HexGame.Board.{getSize, play, printBoard}

object Test extends App {

  val test = BoardState(5)
  var testBoard = test.board
  printBoard(testBoard, Board.getSize(testBoard))

  println("Player red: 1 1")
  testBoard = play(testBoard, Cells.Red, 1, 1)
  printBoard(testBoard, getSize(testBoard))

  println("Player blue: 2 2")
  testBoard = play(testBoard, Cells.Blue, 2, 2)
  printBoard(testBoard, getSize(testBoard))

  println("Player red: 2 2")
  testBoard = play(testBoard, Cells.Red, 2, 2)
  printBoard(testBoard, getSize(testBoard))

  println("Player red: 1 2")
  testBoard = play(testBoard, Cells.Red, 1, 2)
  printBoard(testBoard, getSize(testBoard))
}