package TUI

import Utils.IO_Utils._
import Utils.{Cells, GameState, MyRandom}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.io.AnsiColor.{RED, BLUE, MAGENTA, BOLD, RESET}

object Hex extends App {

  welcomeHeader()
  startGame()

  def startGame(): Unit = {
    val rand = MyRandom(10)
    val startOptions = SortedMap[Int, String](
      0 -> "Exit",
      1 -> "Instructions",
      2 -> "Start new game",
      3 -> "Load saved game"
    )

    val (init, first) = showPrompt(startOptions, rand)
    printBoard(init)


    if (first) {
      print(s"===$BOLD${BLUE}Computer Turn$RESET===\n")
      val newInit = init.computerPlay(Cells.Blue)
      printBoard(newInit)

      mainLoop(newInit)
    }
    else
      mainLoop(init)
  }

  @tailrec
  def mainLoop(gameState: GameState): Unit = {
      val pos_player = getUserInputPosition(gameState, "\nInput move coordinates") // recebe posicao da jogada
      val isUndo = pos_player._1 == -1 && pos_player._2 == -1

      if (isUndo) { // faz undo
        if( gameState.canUndo) { // verifica se pode fazer undo
          val newGameState = gameState.undo
          println(s"\n===$BOLD${MAGENTA}Returning to last play$RESET===\n")
          printBoard(newGameState)
          mainLoop(newGameState)
        }
        else{       // quando nao e possivel fazer undo, faz print a uma mensagem de erro e retorna ao mainloop
          println(s"\n$BOLD${RED}Cannot undo!$RESET\n")
          mainLoop(gameState)
        }
      }
      else {
        val newGameState1 = gameState.playerPlay(Cells.Red, pos_player._1, pos_player._2)
        print(s"\n===$BOLD${RED}Player Turn$RESET===\n")
        printBoard(newGameState1)
        if ( newGameState1.won(Cells.Red)) {
          println(s"\n***$BOLD${RED}YOU WON!$RESET***\n")
          startGame()
        }

        // computador faz a jogada
        val newGameState2 =  newGameState1.computerPlay(Cells.Blue)
        print(s"\n===$BOLD${BLUE}Computer Turn$RESET===\n")
        printBoard(newGameState2)
        if(newGameState2.won(Cells.Blue)) {
            println(s"\n***$BOLD${BLUE}You Lost!$RESET***\n")
            startGame()
        }

        mainLoop(newGameState2)
      }
  }
}
