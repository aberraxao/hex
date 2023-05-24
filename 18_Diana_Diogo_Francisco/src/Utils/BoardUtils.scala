package Utils

import Utils.HexBoard.Board

object BoardUtils {


  // Metodos para inicializar tabuleiro
  private def emptyList(size: Int): List[Cells.Cell] = {  // Inicializa a linha de um tabuleiro com posicoes vazias
    if (size == 0)
      Nil
    else
      Cells.Empty :: emptyList(size - 1)
  }


  private def emptyBoard(size: Int, acc: Int): Board = {    // Inicializa um tabuleiro dado um tamanho
    if (acc == size)
      Nil
    else
      emptyList(size) :: emptyBoard(size, acc + 1)
  }

  def generateEmptyBoard(size: Int): Board = {
    emptyBoard(size, 0)
  }
}

