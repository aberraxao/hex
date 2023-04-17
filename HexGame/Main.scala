package HexGame

import Utils.{IO_Utils, MyRandom, RandomWithState}
import HexGame.Board.{play, printBoard, randomMove, undo}

import scala.annotation.tailrec
import scala.collection.SortedMap

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
    playloop(boardState, r, null)
  }

  @tailrec
  def playloop(boardState: BoardState, r: RandomWithState, prevPlay: ((Int, Int), (Int, Int))): Unit = {
    val newUserPosition = IO_Utils.getUserInputPosition(boardState.board, "Insert next move or undo")
    val isUndo = newUserPosition == (-1, -1)

    if (isUndo) {
      val newUserBoard = new BoardState(undo(boardState.board, prevPlay))
      printBoard(newUserBoard)
      playloop(newUserBoard, r, null)

    } else {
      val newUserBoard = new BoardState(play(boardState.board, Cells.Red, newUserPosition))
      printBoard(newUserBoard)

      val newRand = randomMove(newUserBoard.board, r)
      val nextRandom = newRand._2
      val newPCPosition = newRand._1

      val newPCBoard = new BoardState(play(newUserBoard.board, Cells.Blue, newPCPosition))
      printBoard(newPCBoard)

      playloop(newPCBoard, nextRandom, (newUserPosition, newPCPosition))
    }
  }
}