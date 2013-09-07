import Minesweeper.Model._

object Main extends App {
    val game = new Game()
    
    game.StartNewGame(
        dimensions    = Dimension(8, 8),
        numberOfMines = 10
    )
    //game.MakeMove((1, 2), 'R')
    //game.MakeMove((1, 5), 'R')
    game.MakeMove(
        Move(
             Location   = Location(4, 5),
             MouseClick = MouseClick.Left()
        )
    )
    game.Print()
    println("======================")
    game.PrintUncovered()
}