package Utils
case class StartMenu(name:String, data : Map[String, String]){

}

object StartMenu {

  def printStartMenu(startMenu: StartMenu) = {
    println("Name:" + startMenu.name)
    startMenu.data.toList map (x => println("Key:" + x._1) + " " + println("Value:" + x._2))
    startMenu
  }

}
