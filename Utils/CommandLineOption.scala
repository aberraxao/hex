package Utils


case class CommandLineOption(name: String, exec: StartMenu => StartMenu)
