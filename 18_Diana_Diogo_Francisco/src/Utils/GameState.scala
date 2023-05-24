package Utils

case class GameState(hexBoard: HexBoard, history: List[HexBoard], random: RandomWithState){
  def canUndo: Boolean = GameState.canUndo(history)
  def undo: GameState = GameState.undo(history, random)
  private def updateHistory(): List[HexBoard] = GameState.updateHistory(hexBoard, history)
  def computerPlay( cell: Cells.Cell): GameState = GameState.computerPlay(random,this, cell)
  def computerPlayGUI( cell: Cells.Cell, hard: Boolean): (GameState, Int, Int) = GameState.computerPlayGUI(random,this, cell, hard)
  def playerPlay(cell: Cells.Cell, row: Int, column: Int): GameState = GameState.playerPlay(this, cell, row, column)
  def won(cell: Cells.Cell): Boolean = GameState.won(hexBoard, cell)
}

object GameState {
  private val limitUndo = 1 // variavel que define o limite de undos, devera ser sempre >= 0

  private def canUndo(history: List[HexBoard]): Boolean = history match {
    case Nil => false
    case _ :: _  => true
  }

  // Realiza o undo
  private def undo(history: List[HexBoard], random: RandomWithState): GameState = history match{
    case List(x) => new GameState(x, Nil, random)   // caso tenha apenas um tabuleiro armazenado no historial, carrega-o e limpa o historial
    case x :: xs => new GameState(x, xs, random)    // se tiver mais de um tabuleiro no historial, carrega o que esta na head e mantem a tail no historial
  }

  // atualiza o historial de jogadas sempre que e feita uma jogada por parte do jogador
  // mantem apenas o numero de tabuleiros no historial, de acordo com o limite de undos
  private def updateHistory(oldBoard: HexBoard, history: List[HexBoard]): List[HexBoard] = history match {
    case Nil => if(history.length == limitUndo) Nil else List(oldBoard) // manter apenas se for possivel o modo sem undos
    case x :: Nil => if(history.length == limitUndo) List(oldBoard) else oldBoard :: x :: Nil
    case x :: xs => if (history.length == limitUndo) oldBoard :: x :: xs.init else oldBoard :: x :: xs
  }

  // Contem todos os metodos para a jogada por parte do computador
  private def computerPlay( random: RandomWithState,gameState: GameState,player: Cells.Cell): GameState = {
    val (position, newRandom) = gameState.hexBoard.randomMove(random)
    val newHexBoard = gameState.hexBoard.play(player, position._1, position._2)
    val newGameState = gameState.copy(hexBoard = newHexBoard, random = newRandom)
    newGameState
  }

  // Contem todos os metodos para a jogada por parte do computador, adaptado para o uso na GUI
  private def computerPlayGUI(random: RandomWithState, gameState: GameState, player: Cells.Cell, hard: Boolean): (GameState, Int, Int) = {
    val (position, newRandom) = hard match{
      case true => gameState.hexBoard.hardRandomMove(player, random)
      case false => gameState.hexBoard.randomMove(random)
    }

    val newHexBoard = gameState.hexBoard.play(player, position._1, position._2)
    val newGameState = gameState.copy(hexBoard = newHexBoard, random = newRandom)
    (newGameState, position._1, position._2)
  }

  // Contem todos os metodos para a jogada por parte do jogador
  private def playerPlay(gameState: GameState,player: Cells.Cell, row: Int, col: Int): GameState = {

    val newHexBoard = gameState.hexBoard.play(player, row, col)
    val newGameState = gameState.copy(hexBoard = newHexBoard, history = gameState.updateHistory())
    newGameState
  }

  private def won(hexBoard: HexBoard, cell: Cells.Cell): Boolean = hexBoard.hasContiguousLine(cell)


}
