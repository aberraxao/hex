package Utils

import HexGame.BoardState

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.util.{Failure, Success, Try}

object IO_Utils {

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

  def getUserInputOption(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(scala.io.StdIn.readLine.trim.toInt)
  }

  def getUserInputPosition(msg:String): (Int, Int) = {
    // TODO: validate if we receive 2 valid integers
    // TODO: allow undo
    // TODO: don't skip user player if position is not allowed
    print(msg + ": ")
    val splitInput = scala.io.StdIn.readLine.trim.split(" ")
    val row = splitInput(0).toInt
    val col = splitInput(1).toInt
    (row, col)
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
