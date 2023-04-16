package HexGame

case class BoardGame(size: Int){
}

object BoardGame {
  type Board = List[List[Cells.Cell]]
  type Size = Int
  //type PlayerRandom = Player
 // type PlayerUser = Player

  def updateBoardDimension(size: Int): BoardGame = {
    new BoardGame(size)
  }
}

