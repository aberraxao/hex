package HexGame

import Utils.{CommandLineOption, IO_Utils, MyRandom, RandomWithState}

import scala.annotation.tailrec
import scala.collection.SortedMap

object Main extends App {

  val board: BoardGame = BoardGame(5)
  val size = Int
  val r = MyRandom(10)

  // TODO: remove option 5
  val options = SortedMap[Int, CommandLineOption](
    1 -> Utils.CommandLineOption("Instructions", IO_Utils.printInstructions),
    2 -> Utils.CommandLineOption("Pick board dimension", IO_Utils.printInstructions), // BoardGame.updateBoardDimension(IO_Utils.getUserInputIntIIIII("Dimension"))),
    3 -> Utils.CommandLineOption("Start new game", IO_Utils.printInstructions),
    4 -> Utils.CommandLineOption("Continue saved game", IO_Utils.printInstructions),
    5 -> Utils.CommandLineOption("Print random position", IO_Utils.printInstructions), //, IO_Utils.printRandomPosition(board, r)),
    0 -> new CommandLineOption("Exit", _ => sys.exit)
  )

  mainLoop(board, r)

  @tailrec
  def mainLoop(cont: BoardGame, random: RandomWithState): Unit = {
    IO_Utils.optionPrompt(options) match {
      // has a bug here
      case Some(opt) => val newCont = opt.exec(cont); mainLoop(newCont, random)
      case _ => println("Invalid option"); mainLoop(cont, random)
    }
  }
}