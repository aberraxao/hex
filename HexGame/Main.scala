package HexGame

import Utils.{CommandLineOption, IO_Utils, MyRandom, RandomWithState, StartMenu}

import scala.annotation.tailrec
import scala.collection.SortedMap

object Main extends App {

  val board = BoardState(5)
  val size = Int
  val r = MyRandom(10)
  val cont = StartMenu("Name", Map())

  // TODO: remove option 5
  val options = SortedMap[Int, CommandLineOption](
    1 -> Utils.CommandLineOption("Instructions", IO_Utils.printInstructions),
    2 -> Utils.CommandLineOption("Pick board dimension", IO_Utils.printInstructions), // BoardGame.updateBoardDimension(IO_Utils.getUserInputIntIIIII("Dimension"))),
    3 -> Utils.CommandLineOption("Start new game", IO_Utils.printInstructions),
    4 -> Utils.CommandLineOption("Continue saved game", IO_Utils.printInstructions),
    5 -> Utils.CommandLineOption("Print board", StartMenu.printStartMenu), //, IO_Utils.printRandomPosition(board, r)),
    0 -> new CommandLineOption("Exit", _ => sys.exit)
  )

  mainLoop(cont, r)

  @tailrec
  def mainLoop(opt: StartMenu, random: RandomWithState): Unit = {
    IO_Utils.optionPrompt(options) match {
      case Some(opt) => val newOpt = opt.exec(cont); mainLoop(newOpt, random)
      case _ => println("Invalid option"); mainLoop(opt, random)
    }
  }
}