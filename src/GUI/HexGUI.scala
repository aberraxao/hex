package GUI


//import HexUtils.generateEmptyBoard
import Utils.BoardUtils.generateEmptyBoard
import Utils.{GameState, HexBoard, MyRandom}
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class HexGUI extends Application{
    override def start(primaryStage: Stage): Unit = {
      primaryStage.setTitle("Hex Game")
      primaryStage.setResizable(false)
      val fxmlLoader = new FXMLLoader(getClass.getResource("/GUI/MainMenu.fxml"))
      val mainViewRoot: Parent = fxmlLoader.load()
      val scene = new Scene(mainViewRoot)
      primaryStage.setScene(scene)
      primaryStage.show()
    }
  }
object FxApp {
  val random = MyRandom(10)
  var game: GameState = new GameState( new HexBoard(generateEmptyBoard(5), 5), List[HexBoard](), random )
  
  def main(args: Array[String]): Unit = {
      Application.launch(classOf[HexGUI], args: _*)

  }

}
