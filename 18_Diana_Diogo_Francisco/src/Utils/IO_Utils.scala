package Utils


import Utils.BoardUtils.generateEmptyBoard
import Utils.HexBoard.Board

import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.util.{Failure, Success, Try}

object IO_Utils {

  private val limitSave: Int = 5

  private def getUserInputOption(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(scala.io.StdIn.readLine.trim.toInt)
  }

  def welcomeHeader(): Unit = {
    println("--> Welcome to the HEX game! <--")
  }


  def getUserInputPosition(gameState: GameState, msg: String): (Int, Int) = {
    print(msg + ": ")
    val splitInput = scala.io.StdIn.readLine.trim.split(" ")
    val position = splitInput.toList match {
      case "undo" :: _ => (-1, -1)
      case "Q" :: Nil => sys.exit
      case "q" :: Nil => sys.exit
      case ("s" | "S") :: Nil =>
        doSave(gameState)
        getUserInputPosition(gameState, msg)
      case _ :: Nil => getUserInputPosition(gameState, "Insert a valid move or undo")
      case x :: y :: _ => try {
        (x.toInt, y.toInt)
      } catch {
        case _: NumberFormatException => getUserInputPosition(gameState, "Insert a valid move or undo")
      }
      case _ => null
    }
    if (!gameState.hexBoard.isValidPosition(position) && position != (-1, -1))
      getUserInputPosition(gameState, "Insert a valid move or undo")
    else
      position
  }

  @tailrec
  def showPrompt(options: SortedMap[Int, String], random: RandomWithState): (GameState, Boolean) = { // acrescentado boolean, indica se pc joga primeiro
    options.toList map ((option: (Int, String)) => println(option._1 + ") " + option._2))

    getUserInputOption("Select an option") match {
      case Success(i) => i match {
        case 0 => sys.exit
        case 1 => printInstructions(); showPrompt(options, random)
        case 2 => (new GameState(startNewGame(), List[HexBoard](), random), computerFirst())
        case 3 => doLoad(options, random)
        case _ => println("Invalid number!"); showPrompt(options, random)
      }
      case Failure(_) => println("Invalid number!"); showPrompt(options, random)
    }
  }

  @tailrec
  private def computerFirst(): Boolean = {
    println("\nWho plays first?\n1- Computer\n2- Player")
    getUserInputOption("Select an option") match {
      case Success(i) => i match {
        case 1 => true
        case 2 => false
        case _ => println("Invalid number!"); computerFirst()
      }
      case Failure(_) => println("Invalid number!"); computerFirst()
    }
  }

  private def printWait(): Unit = {
    println("\nPress any key to continue...")
    scala.io.StdIn.readLine()
  }

  private def printInstructions(): Unit = {
    println("\n-- Instructions --" +
      "\nHex is a 2-player board game in which players attempt to connect opposite sides of the board." +
      "\nAvailable commands:" +
      "\nx y: to attempt to move to the position (x,y)" +
      "\nundo: to undo your last move" +
      "\nQ or q: to exit" +
      "\ns or S: to save the game")
    printWait()
  }

  @tailrec
  private def startNewGame(): HexBoard = {
    getUserInputOption("Pick board dimension") match {
      case Success(boardSize: Int) =>
        if (boardSize > 1)
          new HexBoard(generateEmptyBoard(boardSize), boardSize)
        else {
          println("Invalid board dimension!")
          startNewGame()
        }
      case Failure(_) => println("Invalid board dimension!"); startNewGame()
    }
  }

  def printBoard(gameState: GameState): Unit = {

    @tailrec
    def printSpace(n: Int): Unit = {
      if (n > 0) {
        print(" ")
        printSpace(n - 1)
      }
    }

    def cellVal(cell: Cells.Cell): String = cell match {
      case Cells.Red => "\u001B[31mX\u001B[0m"
      case Cells.Blue => "\u001B[34mO\u001B[0m"
      case Cells.Empty => "."
    }

    @tailrec
    def printBoardHeader(row: List[Cells.Cell]): Unit = row match {
      case _ :: Nil => print("\u001B[34m *\u001B[0m\n")
      case _ :: xs => print("\u001B[34m *  \u001B[0m"); printBoardHeader(xs)
    }

    @tailrec
    def printRow(row: List[Cells.Cell]): Unit = row match {
      case x :: Nil => print(cellVal(x) + "\u001B[31m *\u001B[0m\n")
      case x :: xs => print(cellVal(x) + " - "); printRow(xs)
    }


    @tailrec
    def printRow2(row: List[Cells.Cell]): Unit = row match {
      case _ :: Nil => print(" \\ \n")
      case _ :: xs => print(" \\ /"); printRow2(xs)
    }


    @tailrec
    def aux(board2: Board, acc: Int): Unit = {
      board2 match {
        case x :: Nil =>
          printSpace(acc)
          print("\u001B[31m* \u001B[0m")
          printRow(x)
          printSpace(acc + 2)
          printBoardHeader(x)
          println()
        case x :: _ =>
          if (acc == 0)
            printBoardHeader(x)
          printSpace(acc)
          print("\u001B[31m* \u001B[0m")
          printRow(x)
          printSpace(acc + 2)
          printRow2(x)
          aux(board2.tail, acc + 2)
      }
    }

    println("")
    aux(gameState.hexBoard.board, 0)
  }


  // Le o ficheiro que armazena informacao sobre os saves, caso nao exista inicializa-o
  def readSaveInfo(): List[String] = {
    val file = new File("saveInfo.txt")
    if (!file.exists())
      initSaveInfo()

    val bufferedSource = Source.fromFile("saveInfo.txt")
    val fullFile = bufferedSource.getLines.toList
    bufferedSource.close()
    fullFile
  }


  // Funcoes para salvar jogo

  private def writeToFile(hexBoard: HexBoard, random: RandomWithState, file: String): Unit = {
    val pw = new PrintWriter(new File(file))
    val board = hexBoard.board
    val size = hexBoard.size

    pw.println(size + ":" + random)
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        val cell_val = board(i)(j)
        if (cell_val != Cells.Empty)
          pw.println(i + ":" + j + ":" + cell_val)
      }
    }

    pw.close()
  }


  // Funcoes auxiliares aos saves

  // Caso o ficheiro de informacao de saves nao exista
  private def initSaveInfo(): Unit = {
    val pw = new PrintWriter(new File("saveInfo.txt"))

    @tailrec
    def aux(acc: Int): Unit = {
      if (acc < limitSave) {
        pw.println(acc + " - Empty")
        aux(acc + 1)
      }
    }

    aux(0)
    pw.close()
  }

  private def doSave(gameState: GameState): Unit = {
    val lines = readSaveInfo()
    printSaveOption(lines)
    println(limitSave + " - Exit saves")
    val save = chooseSave(lines, true)
    if (save != null) {
      rewriteSaveInfo(save, gameState)
      writeToFile(gameState.hexBoard, gameState.random, "save" + save.split(" - ")(0) + ".txt")
      println("File saved successfully")
    }
    printBoard(gameState)
  }

  @tailrec
  private def chooseSave(lst: List[String], save: Boolean): String = {
    val input = if (save) {
      getUserInputOption("Choose save slot")
    } else {
      getUserInputOption("Choose game to load")
    }

    input match {
      case Success(input) => input match {

        case i if i < limitSave =>
          if (lst(i).split(" - ")(1) == "Empty") { // caso input dado seja em empty
            if (save)
              return lst(i) // se for save devolve a linha indicada
            println("Save is empty, try another one.") // se for load, nao permite
            chooseSave(lst, save)
          }
          else { // se nao for empty
            if (save) { // se for save valida se quer salvar por cima
              println("Are you sure you want to override(Y/N)")
              if (Override())
                lst(i)
              else
                chooseSave(lst, save)
            }
            else // caso seja load, devolve linha indicada
              lst(i)
          }
        case i if i == limitSave => null // caso deseje sair
        case _ => println("Invalid input, try again!"); chooseSave(lst, save)
      }
      case Failure(_) => println("Invalid input, try again!"); chooseSave(lst, save)
    }
  }

  @tailrec
  private def Override(): Boolean = {
    Try(scala.io.StdIn.readLine.trim) match {
      case Success(i) => i match {
        case "y" | "Y" => true
        case "n" | "N" => false
        case _ => println("invalid answer"); Override()
      }
      case Failure(_) => println("invalid answer"); Override()
    }
  }

  private def printSaveOption(lst: List[String]): Unit = {
    println()
    lst.map(x => println(x))
  }

  private def rewriteSaveInfo(line: String, gameState: GameState): Unit = {
    val index = line.split(" - ")(0)
    val f = new File("saveInfo.txt")
    if (!f.exists()) {
      initSaveInfo()
    }

    val bufferedSource = Source.fromFile("saveInfo.txt")
    val fullFile = bufferedSource.getLines.toList
    bufferedSource.close

    val pw = new PrintWriter(new File("saveInfo.txt"))
    val alteredLine = line.split(" - ")(0) + " - Board size: " + gameState.hexBoard.size + " " + LocalDateTime.now()
    fullFile.map(x => if (x.split(" - ")(0) == index) pw.println(alteredLine) else pw.println(x))
    pw.close()
  }


  // Funcoes para carregar jogo

  def doLoad(map: SortedMap[Int, String], random: RandomWithState): (GameState, Boolean) = {
    val lines = readSaveInfo()
    printSaveOption(lines)
    println(limitSave + " - Return")
    val load = chooseSave(lines, false)
    if (load == null)
      showPrompt(map, random)
    else
      (readFromFile("save" + load.split(" - ")(0) + ".txt"), false)
  }

  private def readFromFile(file: String): GameState = {
    val bufferedSource = Source.fromFile(file)
    val fullFile = bufferedSource.getLines.toList
    val firstLine = fullFile.head.split(":")
    val boardInfo = fullFile.tail
    val b = generateEmptyBoard(firstLine(0).toInt)
    val board = new HexBoard(b, firstLine(0).toInt)
    val seed = firstLine(1).split('(')(1).split(')')(0) //asInstanceOf[Utils.MyRandom]
    bufferedSource.close
    val loadedBoard = fileToBoard(board, boardInfo)
    new GameState(loadedBoard, List[HexBoard](), MyRandom(seed.toLong))
  }

  @tailrec
  def fileToBoard(board: HexBoard, list: List[String]): HexBoard = list match {
    case Nil => board
    case x :: xs =>
      val line = x.split(":")
      lazy val cell = line(2) match {
        case "Red" => Cells.Red
        case "Blue" => Cells.Blue
      }
      val newBoard = board.play(cell, line(0).toInt, line(1).toInt)
      fileToBoard(newBoard, xs)
  }
}
