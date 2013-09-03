import Minesweeper.Model

object Main extends App {
	var mf = new Minesweeper.Model.Impl.Minefield (
	      numberOfRows    = 8,
	      numberOfColumns = 8,
	      numberOfMines   = 12
	  )

	mf.PrintCovered()
	println("========================")
	mf.PrintUncovered()
}