package Utils

import HexGame.Board.getPlay
import HexGame.BoardState

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
          savedBoard = new BoardState(getPlay(savedBoard.board, player, (row, col)))
        }
      }
      sf.close
      savedBoard
    }
    catch {
      case _: IOException => println("Erro de leitura do ficheiro"); sys.exit()
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
