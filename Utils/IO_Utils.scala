package Utils

import HexGame.{Board, BoardState}

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.util.{Failure, Success, Try}

object IO_Utils {

  def getUserInputOption(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(scala.io.StdIn.readLine.trim.toInt)
  }

  def promptInt(msg: String): Int = {
    print(msg + ": ")
    scala.io.StdIn.readInt()
  }

  @tailrec
  def showPrompt(options: SortedMap[Int, String]): Unit = {
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
    val boardSize = promptInt("Pick board dimension")
    BoardState(boardSize)
  }

  def continueGame(): Unit = {
    println("TODO CONTINUE NEW GAME")
    printWait()
  }

}
