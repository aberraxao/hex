package HexGame

import scala.collection.SortedMap
import Utils.{IO_Utils, MyRandom}
import HexGame.Board.printBoard


object Main extends App {

  val r = MyRandom(10)

  val options = SortedMap[Int, String](
    0 -> "Exit",
    1 -> "Instructions",
    2 -> "Start new game",
    3 -> "Continue saved game"
  )

  main()

  def main(): Unit = {
    val boardState = IO_Utils.showPrompt(options)
    printBoard(boardState)
  }
}