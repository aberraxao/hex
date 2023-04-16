package Utils

import HexGame.BoardState

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.util.{Failure, Success, Try}

object IO_Utils {

  def getUserInputOption(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(scala.io.StdIn.readLine.trim.toInt)
  }

  @tailrec
  def showPrompt(options: SortedMap[Int, String]): BoardState = {
    println("--> Welcome to the HEX game! <--")
    options.toList map ((option: (Int, String)) => println(option._1 + ") " + option._2))

    getUserInputOption("Select an option") match {
      case Success(i) => i match {
        case 0 => sys.exit
        case 1 => printInstructions(); showPrompt(options)
        case 2 => startNewGame()
        case 3 => continueGame()
      }
      case Failure(_) => println("Invalid number!"); showPrompt(options)
    }
  }

  def printWait(): Unit = {
    println("Press any key to continue...")
    scala.io.StdIn.readLine()
  }

  def printInstructions(): Unit = {
    println("-- Instructions --" +
      "\nHex is a 2-player board game in which players attempt to connect opposite sides of the board." +
      "\nAvailable commands:" +
      "\nx y: to attempt to move to the position (x,y)" +
      "\nundo: to undo your last move" +
      "\nexit: to save the game and exit")
    printWait()
  }

  def startNewGame(): BoardState = {
    getUserInputOption("Pick board dimension") match {
      case Success(boardSize) => new BoardState(boardSize);
      case Failure(_) => println("Invalid board dimension!"); startNewGame()
    }
  }

  def continueGame(): BoardState = {
    println("TODO CONTINUE NEW GAME")
    printWait()
    new BoardState(5)
  }

}
