import Minesweeper.Model.Impl._

object Main extends App {
  val game = new Game()
  
  game.StartNewGame((8, 8), 10)
  game.MakeMove((1, 2), 'R')
  game.MakeMove((1, 5), 'R')
  game.Print()
  println("======================")
  game.MakeMove((4, 5), 'L')
  game.Print()
}