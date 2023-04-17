package Utils

import HexGame.Board.{Board, play}
import HexGame.{BoardState, Cells}

import java.io.{File, FileWriter, IOException}
import scala.io.Source

object File_Utils {

  def readCsv(fileName: String): BoardState = {

    try {
      val sf = Source.fromFile(fileName)
      val boardSize = sf.getLines().next().toInt
      var savedBoard = new BoardState(boardSize) // TODO: replace with a val
      for ((line, row) <- sf.getLines.zipWithIndex) {
        val splitRow = line.split(",")
        for ((player, col) <- splitRow.zipWithIndex) {
          savedBoard = new BoardState(updateSavedBoard(player, savedBoard, row, col))
        }
      }
      sf.close
      savedBoard
    }
    catch {
      case e: IOException => println("Erro de leitura do ficheiro"); sys.exit()
    }
  }

  def updateSavedBoard(player: String, savedBoard: BoardState, row: Int, col: Int): Board = {
    player match {
      case "Red" => play(savedBoard.board, Cells.Red, (row, col))
      case "Blue" => play(savedBoard.board, Cells.Blue, (row, col))
      case "Empty" => savedBoard.board
      case _ => println("Há um erro no ficheiro"); savedBoard.board
    }
  }

  def saveCsv(fileName: String, boardState: BoardState): Unit = {

    val fw = new FileWriter(new File(fileName))

    fw.write(boardState.boardSize.toString + "\n")

    for (row <- boardState.board) {
      for (col <- row)
        fw.write(col.toString + ",")
      fw.write("\n")
    }
    fw.close()
  }
}
