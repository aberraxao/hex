package Utils

import Utils.HexBoard.Board
import Utils.RandomWithState

import scala.annotation.tailrec

case class HexBoard(board: Board, size: Int){
  def randomMove(rand: RandomWithState): ((Int, Int), RandomWithState) = HexBoard.randomMove(board, rand)
  def cellPositions(cell: Cells.Cell): List[(Int, Int)] = HexBoard.cellPositions(board, cell)
  def play(cell: Cells.Cell, row: Int, column: Int): HexBoard = HexBoard.play(this, cell, row, column)
  def hasContiguousLine(cell: Cells.Cell): Boolean = HexBoard.hasContiguousLine(cell, board, size)
  def isValidPosition(position: (Int, Int)): Boolean = HexBoard.isValidPosition(this, position)
  def hardRandomMove(cell: Cells.Cell, rand: RandomWithState): ((Int, Int), RandomWithState) = HexBoard.hardRandomMove(board, cell, rand)
}

object HexBoard {
  type Board = List[List[Cells.Cell]]

  // T1
  private def randomMove(board: Board, rand: RandomWithState): ((Int, Int), RandomWithState) = {
    val emptyPosLst = cellPositions(board, Cells.Empty) // armazena uma lista com todas as posicoes vazias do tabuleiro
    val len = emptyPosLst.length
    val (i1, newRand) = rand.nextInt(len)               // gera um indice aleatorio para essa mesma lista
    (emptyPosLst(i1), newRand)                          // devolve uma posicao aleatoria da lista e um novo random
  }

  private def cellPositions(board: Board, cell: Cells.Cell): List[(Int, Int)] = {
    def aux(b: Board, r: Int): List[(Int, Int)] = b match { // precorre linhas do tabuleiro
      case Nil => Nil
      case x :: xs => aux2(x, r, 0) ::: aux(xs, r + 1)// precorre para cada linha todas as colunas
    }

    def aux2(l: List[Cells.Cell], r: Int, acc: Int): List[(Int,Int)] = l match { // precorre colunas e devolve posicao dos elementos que sao empty
      case Nil => Nil
      case y :: ys => if(y == cell) (r, acc) :: aux2(ys, r, acc + 1) else aux2(ys, r, acc + 1)
    }

    aux(board, 0)
  }

  private def hardRandomMove(board: Board, cell: Cells.Cell, random: RandomWithState): ((Int, Int), RandomWithState) =  {
    val emptyPositions = cellPositions(board, Cells.Empty) // armazena posicoes vazias do tabuleiro
    val computerPositions = cellPositions(board, cell)   // armazena posicoes jogadas pelo computador no tabuleiro

    @tailrec
    def aux(computerPos: List[(Int,Int)], emptyPositions: List[(Int,Int)], rand: RandomWithState): (List[(Int,Int)], RandomWithState) = {
      if(computerPos.isEmpty)   // caso nao haja/sobre qualquer jogada feita pelo computador devole a lista de posicoes vazias para jogar
        return (emptyPositions, rand)

      val (index, newRand) = rand.nextInt(computerPos.length)
      val randomPos = computerPos(index)
      lazy val relatedPositions = emptyPositions.filter(y => isNeighbor(randomPos, y)) // filtra os pontos conexos
      if(relatedPositions.isEmpty)    // no caso de nao haver pontos conexos para o ponto escolhido aleatoriamente, procura para outros
        aux(computerPos.filterNot(_ == randomPos), emptyPositions, newRand)
      else    // devolve a lista com as posicoes conexas ao ponto escolhido aleatoriamente
        (relatedPositions, newRand)
    }

    val (relatedPositions, newRandom) = aux(computerPositions, emptyPositions, random)
    val len = relatedPositions.length
    val (i1, newRandom2) = newRandom.nextInt(len) // escolhe um dos pontos conexos aleatoriamente
    (relatedPositions(i1), newRandom2)
  }

  // T2
  private def play(hexBoard: HexBoard, player: Cells.Cell, row: Int, col: Int): HexBoard = {

    def aux(): Board = {                  // precorre todas as linhas do tabuleiro
      (hexBoard.board foldRight List[List[Cells.Cell]]()) { (h, t) =>
        val r = hexBoard.size - t.length - 1  // guarda o indice da linha
        aux2(h, r) :: t                   // para cada linha do tabuleiro ve cada elemento e altera na posicao dada
      }
    }

    def aux2(row2: List[Cells.Cell], r: Int): List[Cells.Cell] = {
      (row2 foldRight List[Cells.Cell]()) { (h, t) =>
        val c = hexBoard.size - t.length - 1  // guarda o indice da coluna
        if (r == row && c == col)         // se a posicao de pesquisa for igual a posicao dada, altera-a
          player :: t
        else                              // caso contrario mantem o mesmo valor
          h :: t
      }
    }

    hexBoard.copy(board = aux())
  }

  private def isValidPosition(hexBoard: HexBoard, pos: (Int,Int)): Boolean ={
    if(pos._1 >= hexBoard.size || pos._2 >= hexBoard.size || pos._1 < 0 || pos._2 < 0 )
      false
    else
      hexBoard.board(pos._1)(pos._2) == Cells.Empty
  }

  private def startUnmarked(lst: List[(Int, Int)]): List[((Int, Int),Boolean)] =
    (lst foldLeft List[((Int, Int), Boolean)]()) ((t, h) => (h, false) :: t)
  // inicializa a lista com todos os pontos a nao visitados


  private def mark(markedList: List[((Int, Int), Boolean)], pos: (Int, Int)): List[((Int, Int), Boolean)] =
    (markedList foldRight List[((Int, Int), Boolean)]())((h,t) => if(h._1 == pos) (h._1, true) :: t else h :: t)
  // marca uma dada posicao como visitada

  private def getNeighbors(list: List[((Int, Int), Boolean)], Pos: (Int, Int)): List[(Int, Int)] = {
    (list filter (x => isNeighbor(Pos, x._1) && !x._2)).map(x => x._1)
  }
  // devolve uma lista com todas as posicoes conexas do tabuleiro

  private def isNeighbor(Pos1: (Int, Int), Pos2: (Int, Int)): Boolean = {
    if (Pos1._1 == Pos2._1)
      Pos1._2 == Pos2._2 - 1 || Pos1._2 == Pos2._2 + 1
    else {
      if (Pos1._1 == Pos2._1 - 1)
        Pos1._2 == Pos2._2 || Pos1._2 == Pos2._2 + 1
      else if (Pos1._1 == Pos2._1 + 1)
        Pos1._2 == Pos2._2 || Pos1._2 == Pos2._2 - 1
      else
        false
    }
  }
  // verifica se um ponto e conexo a outro

  @tailrec
  private def DFS(invertedStack: List[(Int, Int)], lst: List[((Int, Int), Boolean)], target: List[(Int, Int)]): Boolean = invertedStack match {
    case Nil => false
    case x :: Nil =>
      if (target.contains(x))     // verifica se a posicao a ser avaliada faz parte da lista "alvo"
        true
      else                        // caso contrario marca o elemento, adiciona todas as posicoes conexas a stack e procede com a procura
        DFS(getNeighbors(lst, x), mark(lst, x), target)
    case x :: xs =>
      if (target.contains(x))
        true
      else
        DFS(getNeighbors(lst, x) ::: xs, mark(lst, x), target)
  }
  // Efetua a pesquisa em profundidade, marcando todas as posicoes visitadas e metendo na stack todas conexas

  // T4
  private def hasContiguousLine(cell: Cells.Cell, board: Board, size: Int): Boolean = {
    val lst = cellPositions(board, cell)      // Inicializa a lista de posicoes para pesquisa
    lazy val markedCells = startUnmarked(lst) // inicializa todas as posicoes como nao visitadas
    lazy val initCells = cell match{          // guarda numa lista todas as posicoes iniciais de procura
      case Cells.Red => lst filter( x => x._2 == 0)
      case Cells.Blue => lst filter( x => x._1 == 0)
    }

    lazy val targetCells = cell match {       // guarda numa lista todas as posicoes "alvo"
      case Cells.Red => lst filter (x => x._2 == size - 1)
      case Cells.Blue => lst filter (x => x._1 == size - 1)
    }

    if(initCells.isEmpty)                     // caso a lista de posicoes iniciais esteja vazia termina logo com false
      false
    else{
      DFS(initCells, markedCells, targetCells)
    }
  }
}
