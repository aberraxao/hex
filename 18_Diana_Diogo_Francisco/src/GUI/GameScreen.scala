package GUI

import GUI.SaveLoadGUI.{readSaveInfoGUI, rewriteSaveInfoGUI, writeToFileGUI}
import Utils.BoardUtils.generateEmptyBoard
import Utils.{Cells, GameState, HexBoard}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, CheckBox, ChoiceBox, Label}
import javafx.scene.input.MouseEvent
import javafx.scene.layout.{AnchorPane, Pane}
import javafx.scene.paint.Color
import javafx.scene.shape.Polygon
import javafx.scene.{Node, Parent}

import scala.collection.JavaConverters.asScalaBufferConverter


class GameScreen() {


  @FXML
  private var windowBlock: Pane = _
  @FXML
  private var Window2: AnchorPane = _
  @FXML
  private var undoButton: Button = _
  @FXML
  private var saveButton: Button = _
  @FXML
  private var mainMenuButton: Button = _
  @FXML
  private var coverBoard: Pane = _
  @FXML
  private var windowCover: Pane = _
  @FXML
  private var saveScreen: Pane = _
  @FXML
  private var saveChoiceBox: ChoiceBox[String] = _
  @FXML
  private var polyPlayer: Polygon = _
  @FXML
  private var polyComputer: Polygon = _
  @FXML
  private var winLabel: Label = _
  @FXML
  private var loseLabel: Label = _
  @FXML
  private var hard: CheckBox = _
  @FXML
  private var mainMenu: Button = _

  def initialize(): Unit = {
    repaint()
  }

  def init(playerColor: Color, computerColor: Color, hardDifficulty: Boolean, first: Boolean): Unit = {
    FxApp.game = new GameState(new HexBoard(generateEmptyBoard(5), 5), List[HexBoard](), FxApp.game.random)
    polyPlayer.setFill(playerColor)
    polyComputer.setFill(computerColor)
    if (hardDifficulty)
      hard.setSelected(true)
    if (first) {
      val ((computerColor, _), (computerCell, _)) = getColors()
      computerPlay(computerColor, computerCell)

    }
  }

  def initLoad(playerColor: Color, computerColor: Color, hardDifficulty: Boolean): Unit = {
    polyPlayer.setFill(playerColor)
    polyComputer.setFill(computerColor)
    hard.setSelected(true)
  }

  def doUndoButtonClicked(): Unit = {
    FxApp.game = FxApp.game.undo
    repaint()
    if (!FxApp.game.canUndo) undoButton.setDisable(true)
    if (coverBoard.isDisable) {
      coverBoard.setDisable(false)
      saveButton.setDisable(false)
    }
  }

  private def repaint(): Unit = {
    clearBoard()
    val redCells = FxApp.game.hexBoard.cellPositions(Cells.Red)
    val blueCells = FxApp.game.hexBoard.cellPositions(Cells.Blue)

    val hexagonsToPaintRed: List[Node] = redCells.map(x => Window2.lookup("#hexagon" + x._1 + x._2))
    val hexagonsToPaintBlue: List[Node] = blueCells.map(x => Window2.lookup("#hexagon" + x._1 + x._2))

    hexagonsToPaintRed.map(x => x.asInstanceOf[Polygon].setFill(Color.RED))
    hexagonsToPaintBlue.map(x => x.asInstanceOf[Polygon].setFill(Color.BLUE))
  }

  private def clearBoard(): Unit = {
    val nodes: List[javafx.scene.Node] = coverBoard.getChildren.asScala.toList

    nodes.map(x => if (x.isInstanceOf[Polygon] && x.getId != null) x.asInstanceOf[Polygon].setFill(Color.WHITE))
  }


  def onHexagonClicked(event: MouseEvent): Unit = {
    val node = event.getTarget.asInstanceOf[Polygon]
    val ((computerColor, playerColor), (computerCell, playerCell)) = getColors()

    //Valida a posicao
    if (node.getFill == Color.WHITE) {
      // Jogada por parte do jogador
      val nodeName = node.getId()
      val x = nodeName(nodeName.length - 2).toInt - 48
      val y = nodeName(nodeName.length - 1).toInt - 48
      FxApp.game = FxApp.game.playerPlay(playerCell, x, y)
      node.setFill(playerColor)
      if (FxApp.game.won(playerCell))
        winScreen()
      else {
        computerPlay(computerColor, computerCell)
        if (FxApp.game.canUndo) {
          undoButton.setDisable(false)
        }
      }
    }
  }

  private def getColors(): ((Color, Color), (Cells.Cell, Cells.Cell)) = {
    if (polyPlayer.getFill == Color.RED)
      ((Color.BLUE, Color.RED), (Cells.Blue, Cells.Red))
    else
      ((Color.RED, Color.BLUE), (Cells.Red, Cells.Blue))
  }

  private def computerPlay(computerColor: Color, computerCell: Cells.Cell): Unit = {
    val computerPlay = FxApp.game.computerPlayGUI(computerCell, hard.isSelected)
    FxApp.game = computerPlay._1
    val targetID = "#hexagon" + computerPlay._2 + computerPlay._3
    val targetHexagon = Window2.lookup(targetID)
    targetHexagon.asInstanceOf[Polygon].setFill(computerColor)

    if (FxApp.game.won(computerCell))
      loseScreen()
  }


  private def loseScreen(): Unit = {
    coverBoard.setDisable(true)
    setWindowCover(true)
    loseLabel.toFront()
    loseLabel.setDisable(false)
    loseLabel.setVisible(true)
    saveButton.setDisable(true)
  }

  private def winScreen(): Unit = {
    coverBoard.setDisable(true)
    setWindowCover(true)
    winLabel.toFront()
    winLabel.setDisable(false)
    winLabel.setVisible(true)
    undoButton.setDisable(true)
    saveButton.setDisable(true)
  }

  def onSelectSaveClicked(): Unit = {
    if (saveChoiceBox.getValue() != null) {
      val saveSlot = saveChoiceBox.getValue()
      val saveFile = "saveGUI" + saveSlot.split(" - ")(0) + ".txt"
      writeToFileGUI(FxApp.game.hexBoard, FxApp.random, hard.isSelected, polyPlayer.getFill == Color.RED, saveFile)
      rewriteSaveInfoGUI(saveSlot, hard.isSelected) // re-escreve o ficheiro com os dados atualizados
      saveChoiceBox.getItems.clear() // faz reset a choice box
      val fullFile = readSaveInfoGUI()
      fullFile.map(x => saveChoiceBox.getItems().add(x)) // atualiza a choice box
    }
  }

  def paint(playerColor: Color, gameColor: Color): Unit = {
    polyComputer.setFill(gameColor)
    polyPlayer.setFill(playerColor)
  }

  def continueOnClicked(): Unit = {
    setWindowCover(false)
    winLabel.setDisable(true)
    winLabel.setVisible(false)
    loseLabel.setDisable(true)
    loseLabel.setVisible(false)
  }

  private def setWindowCover(show: Boolean): Unit = {
    if (show) {
      windowCover.setDisable(false)
      windowCover.setVisible(true)
      windowCover.toFront()
    } else {
      windowCover.setDisable(true)
      windowCover.setVisible(false)
    }
  }

  private def setWindowBlock(show: Boolean): Unit = {
    if (show) {
      windowBlock.setVisible(true)
      windowBlock.toFront()
    } else {
      windowBlock.setVisible(false)
    }
  }

  def onSaveButtonClicked(): Unit = {
    saveChoiceBox.getItems.clear()
    windowBlock.toFront()
    setWindowBlock(true)
    saveScreen.toFront()
    saveScreen.setVisible(true)
    val fullFile = readSaveInfoGUI()
    fullFile.map(x => saveChoiceBox.getItems().add(x))
  }

  def returnSaveOnClicked(): Unit = {
    setWindowBlock(false)
    saveScreen.setVisible(false)

  }

  def onMainMenuClicked(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("MainMenu.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    mainMenu.getScene.setRoot(mainViewRoot)
  }
}
