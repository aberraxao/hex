package Utils

import HexGame.BoardGame

case class CommandLineOption(name: String, exec: BoardGame => BoardGame)