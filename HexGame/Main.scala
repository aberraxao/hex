package HexGame

import scala.collection.SortedMap
import Utils.{IO_Utils, MyRandom, RandomWithState}
import HexGame.Board.{play, printBoard, randomMove}


object Main extends App {

  val options = SortedMap[Int, String](
    0 -> "Exit",
    1 -> "Instructions",
    2 -> "Start new game",
    3 -> "Continue saved game"
  )

  main()

  def main(): Unit = {
    val boardState = IO_Utils.showPrompt(options)
    val r = MyRandom(2023)
    printBoard(boardState)
    playloop(boardState, r)
  }

  def playloop(boardState: BoardState, r:RandomWithState): Unit = {
    val position = IO_Utils.getUserInputPosition("Insert next move")
    val newUserBoard = new BoardState(play(boardState.board, Cells.Red, position))
    printBoard(newUserBoard)

    val newRand = randomMove(newUserBoard.board, r)
    val newPCBoard = new BoardState(play(newUserBoard.board, Cells.Blue, newRand._1))
    printBoard(newPCBoard)

    playloop(newPCBoard, newRand._2)
  }
}