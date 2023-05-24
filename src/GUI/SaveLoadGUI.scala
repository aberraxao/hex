package GUI

import Utils.BoardUtils.generateEmptyBoard
import Utils.IO_Utils.{fileToBoard, initSaveInfo}
import Utils.{Cells, GameState, HexBoard, MyRandom, RandomWithState}

import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import scala.io.Source

object SaveLoadGUI {

  // SAVE
  private def initSaveInfoGUI(): Unit = {
    val pw = new PrintWriter(new File("saveInfoGUI.txt"))

    def aux(acc: Int): Unit = {
      if (acc < 5) {
        pw.println(acc + " - Empty")
        aux(acc + 1)
      }
    }

    aux(0)
    pw.close()
  }

  def readSaveInfoGUI(): List[String] = {
    val file = new File("saveInfoGUI.txt")
    if (!file.exists())
      initSaveInfoGUI()

    val bufferedSource = Source.fromFile("saveInfoGUI.txt")
    val fullFile = bufferedSource.getLines.toList
    bufferedSource.close

    fullFile
  }

  def rewriteSaveInfoGUI(line: String, hard: Boolean): Unit = {
    val index = line.split(" - ")(0)
    val f = new File("saveInfoGUI.txt")
    if (!f.exists()) {
      initSaveInfoGUI()
    }

    val fullFile = readSaveInfoGUI()
    val difficulty = hard match{
      case true => "Hard"
      case false => "Normal"
    }
    val pw = new PrintWriter(new File("saveInfoGUI.txt"))
    val alteredLine = line.split(" - ")(0) + " - " + difficulty + " " + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    fullFile.map(x => if (x.split(" - ")(0) == index) pw.println(alteredLine) else pw.println(x))
    pw.close()
  }

  def writeToFileGUI(hexBoard: HexBoard, random: RandomWithState, hardDifficulty: Boolean, isRed: Boolean, file: String): Unit = {
    val pw = new PrintWriter(new File(file))
    val board = hexBoard.board
    val size = hexBoard.size


    pw.println(size + ":" + random + ":" + hardDifficulty + ":" + isRed)
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        val cell_val = board(i)(j)
        if (cell_val != Cells.Empty)
          pw.println(i + ":" + j + ":" + cell_val)
      }
    }

    pw.close()
  }


  // LOAD

  def readFromFileGUI(file: String): (GameState, Boolean, Boolean) = {
    val bufferedSource = Source.fromFile(file)
    val fullFile = bufferedSource.getLines.toList
    val firstLine = fullFile.head.split(":")
    val boardInfo = fullFile.tail
    val b = generateEmptyBoard(firstLine(0).toInt)
    val board = new HexBoard(b, firstLine(0).toInt)
    val seed = firstLine(1).split('(')(1).split(')')(0) //asInstanceOf[Utils.MyRandom]
    val isHardMode = firstLine(2).toBoolean
    val isRed = firstLine(3).toBoolean
    bufferedSource.close
    val loadedBoard = fileToBoard(board, boardInfo)
    val gameState = new GameState(loadedBoard, List[HexBoard](), MyRandom(seed.toLong))
    (gameState, isHardMode, isRed)
  }

}
