package Utils

import HexGame.Board.{Board, isValidPlay, readBoard}
import HexGame.{BoardState, Cells}

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

  def getUserInputPosition(board: Board, msg: String): (Int, Int) = {
    print(msg + ": ")
    val splitInput = scala.io.StdIn.readLine.trim.split(" ")
    val position = splitInput.toList match {
      case "undo" :: _ => (-1, -1)
      case x :: y :: _ => try {
        (x.toInt, y.toInt)
      } catch {
        case _: NumberFormatException => getUserInputPosition(board, "Insert a valid move or undo")
      }
      case _ => null
    }
    if (!isValidPlay(board, position) && position != (-1,-1))
      getUserInputPosition(board, "Insert a valid move or undo")
    else
      position
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
    readBoard("save.csv")
  }
}
