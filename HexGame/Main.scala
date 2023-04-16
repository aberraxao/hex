package HexGame

import scala.collection.SortedMap
import Utils.{IO_Utils, MyRandom, RandomWithState}
import HexGame.Board.{play, printBoard, randomMove, undo}


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

  def playloop(boardState: BoardState, r: RandomWithState, prevPlay: ((Int, Int), (Int, Int))): Unit = {
    val position = IO_Utils.getUserInputPosition(boardState.board, "Insert next move or undo")
    val isUndo = Board.checkPosition(boardState.board, position) == 1
    val newUserBoard =
      if (isUndo) {
        new BoardState(undo(boardState.board, prevPlay))
      }
      else {
        new BoardState(play(boardState.board, Cells.Red, position))
      }
    printBoard(newUserBoard)

    if(isUndo) {
      playloop(newUserBoard, r, null)
    }
    else {
      val newRand = randomMove(newUserBoard.board, r)
      val newPCBoard = new BoardState(play(newUserBoard.board, Cells.Blue, newRand._1))
      printBoard(newPCBoard)

      val newPlay = (position, newRand._1)

      playloop(newPCBoard, newRand._2, newPlay)
    }
  }
}