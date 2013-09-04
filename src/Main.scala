import Minesweeper.Model.Impl._

object Main extends App {
  var game = new Game()
  
  game.StartNewGame((8, 8), 10)
  game.MakeMove((1,2), 'R')
  game.MakeMove((1,4), 'R')
  game.Print()
}