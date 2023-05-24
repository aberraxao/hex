package GUI

import GUI.SaveLoadGUI.{readFromFileGUI, readSaveInfoGUI, rewriteSaveInfoGUI, writeToFileGUI}
import Utils.BoardUtils.*
import Utils.{Cells, GameState, HexBoard}
import Utils.IO_Utils.{readFromFile, readSaveInfo, writeToFile}
import javafx.application.Platform
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.*
import javafx.scene.input.MouseEvent
import javafx.scene.layout.{AnchorPane, FlowPane, GridPane, Pane}
import javafx.scene.paint.*
import javafx.scene.shape.*
import javafx.scene.{Node, Parent}


class Controller {


  @FXML
  private var playButton: Button = _
  @FXML
  private var resetButton: Button = _
  @FXML
  private var Window: AnchorPane = _
  @FXML
  private var coverSettings: Pane = _
  @FXML
  private var windowBlock: Pane = _
  @FXML
  private var menu: GridPane = _
  @FXML
  private var Red: RadioButton = _
  @FXML
  private var Blue: RadioButton = _
  @FXML
  private var loadButton: Button = _
  @FXML
  private var hardCheckBox: CheckBox = _
  @FXML
  private var computerFirstCheckBox: CheckBox = _
  @FXML
  private var loadScreen: Pane = _
  @FXML
  private var loadChoiceBox: ChoiceBox[String] = _
  @FXML
  private var startButton: Button = _
  @FXML
  private var polyColor: Polygon = _
  @FXML
  private var Return: Button = _
  @FXML
  private var selectLoad: Button = _

  private def getColorsInit: ((Color, Color), (Cells.Cell, Cells.Cell)) = {
    if (Red.isSelected)
      ((Color.BLUE, Color.RED), (Cells.Blue, Cells.Red))
    else
      ((Color.RED, Color.BLUE), (Cells.Red, Cells.Blue))
  }

  def onRedRadioButtonClicked(): Unit = {
    polyColor.setFill(Color.RED)
  }

  def onBlueRadioButtonClicked(): Unit = {
    polyColor.setFill(Color.BLUE)
  }


  def onQuitButtonClicked(): Unit = {
    Platform.exit()
  }

  def onReturnButtonClicked(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("MainMenu.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    Return.getScene.setRoot(mainViewRoot)
  }


  def onPlayButtonClicked(): Unit = {
    val ((computerColor, playerColor), (computerCell, playerCell)) = getColorsInit
    val fxmlLoader = new FXMLLoader(getClass.getResource("GameScreen.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val control = fxmlLoader.getController[GameScreen]()
    control.init(playerColor, computerColor, hardCheckBox.isSelected, computerFirstCheckBox.isSelected)

    playButton.getScene.setRoot(mainViewRoot)
  }

  def onResetButtonClicked(): Unit = {
    playButton.setDisable(false)
    resetButton.setDisable(true)
    loadButton.setDisable(false)

    coverSettings.toBack()

  }

  // LOAD

  def onLoadButtonClicked(): Unit = {
    loadChoiceBox.getItems.clear()
    val fullFile = readSaveInfoGUI()
    fullFile.map(x => if (x.split(" - ")(1) != "Empty") loadChoiceBox.getItems().add(x)) // atualiza a choice box apenas com slots que tenham save

    windowBlock.setDisable(false)
    windowBlock.setVisible(true)
    windowBlock.toFront()
    setWindowBlock(true)
    loadScreen.toFront()
    loadScreen.setVisible(true)

  }

  def returnLoadOnClicked(): Unit = {
    setWindowBlock(false)
    loadScreen.setVisible(false)
  }

  def onSelectLoadButtonClicked(): Unit = {
    val chosenSlot = loadChoiceBox.getValue
    if (chosenSlot != null) {
      val file = "saveGUI" + chosenSlot.split(" - ")(0) + ".txt"
      val (game, hard, isRed) = readFromFileGUI(file)

      FxApp.game = game
      val fxmlLoader = new FXMLLoader(getClass.getResource("GameScreen.fxml"))
      val mainViewRoot: Parent = fxmlLoader.load()
      val control = fxmlLoader.getController[GameScreen]()

      selectLoad.getScene.setRoot(mainViewRoot)
      val playerColor = if (isRed) Color.RED else Color.BLUE
      val computerColor = if (isRed) Color.BLUE else Color.RED
      control.initLoad(playerColor, computerColor, hard)
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

  def onStartButtonClicked(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("Settings.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    startButton.getScene.setRoot(mainViewRoot)
  }


}
