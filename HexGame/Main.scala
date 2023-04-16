package HexGame

import scala.collection.SortedMap
import Utils.{IO_Utils, MyRandom}


object Main extends App {

  val board = BoardState(5)
  val r = MyRandom(10)


  val options = SortedMap[Int, String](
    0 -> "Exit",
    1 -> "Instructions",
    2 -> "Start new game",
    3 -> "Continue saved game"
  )

  mainLoop(board)

  def mainLoop(boardState: BoardState): Unit = {
    IO_Utils.showPrompt(options)
  }
}