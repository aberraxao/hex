package Utils

import HexGame.{Board, BoardState}

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object IO_Utils {

  def getUserInputInt(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(readLine.trim.toUpperCase.toInt)
  }

  def getUserInputIntIIIII(msg: String): Int = {
    print(msg + ": ")
    readLine.trim.toUpperCase.toInt
  }

  def prompt(msg: String): String = {
    print(msg + ": ")
    scala.io.StdIn.readLine()
  }

  @tailrec
  def optionPrompt(options: SortedMap[Int, CommandLineOption]): Option[CommandLineOption] = {
    println("--> Welcome to the HEX game! <--")
    println("-- Options --")
    options.toList map ((option: (Int, CommandLineOption)) => println(option._1 + ") " + option._2.name))

    getUserInputInt("Select an option") match {
      case Success(i) => options.get(i)
      case Failure(_) => println("Invalid number!"); optionPrompt(options)
    }
  }

  def printInstructions(board: StartMenu): StartMenu = {
    println("-- Instructions --" +
      "\nHex is a 2-player board game in which players attempt to connect opposite sides of the board." +
      "\nAvailable commands:" +
      "\nx y: to attempt to move to the position (x,y)" +
      "\nundo: to undo your last move" +
      "\nexit: to save the game and exit")
    board
  }
  /*
  def printBoard(board: StartMenu): StartMenu = {
    Board.printBoard(board)
    board
  }

  def printRandomPosition(board: StartMenu, r: MyRandom): StartMenu = {
    println(r)
    board
  }
  */

}
