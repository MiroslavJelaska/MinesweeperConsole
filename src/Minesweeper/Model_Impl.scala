package Minesweeper.Model.Impl

import Minesweeper.Model;

case class Move
(
  Row:        Int,
  Column:     Int,
  MouseClick: Model.MouseClick.type
)
{
  
}

class Game
(
    
)
{
    private var _minefield: Option[Minesweeper.Model.Impl.Minefield] = None
    
    def StartNewGame(dimensions: (Int, Int), numberOfMines: Int): Unit =
    {
      _minefield = Some(new Minesweeper.Model.Impl.Minefield(
        numberOfRows    = dimensions._1,
        numberOfColumns = dimensions._2,
        numberOfMines   = numberOfMines
      ))
    }
    
    def MakeMove(location: (Int, Int), click: Char) =
    {
      require(!_minefield.isEmpty)
      require(click == 'L' || click == 'R')
      
      val move =  new Move(
          Row        = location._1,
          Column     = location._2,
          MouseClick = click match {
              case 'L' => Model.MouseClick.Left
              case 'R' => Model.MouseClick.Right
            }
      )
      _minefield.get.MakeMove(move)
    }
    
    def Print(): Unit =
    {
      require(!_minefield.isEmpty)
      
      _minefield.get.Print()
    }
    
    def EndGame()
    {
      _minefield = None
    }
}

class Minefield
(
  numberOfRows:    Int,
  numberOfColumns: Int,
  numberOfMines:   Int
)
{
  require(numberOfRows    > 2)
  require(numberOfColumns > 2)
  
  private val _fields = Array.fill(numberOfRows, numberOfColumns)(MineSquare(false))
  _fillWithMines(numberOfMines)
  
  private def _fillWithMines(numberOfMines: Int): Unit =
  {
    def getRandomFlattenedLocation = scala.util.Random.nextInt(numberOfRows * numberOfColumns)
    
    def randomFill(flatenedLocation: Int): Unit =
    {
      if(!_fields(flatenedLocation / numberOfRows)(flatenedLocation % numberOfRows).HasMine)
      {
        _fields(flatenedLocation / numberOfRows)(flatenedLocation % numberOfRows) = MineSquare(true)
      }
      else
      {
          randomFill(getRandomFlattenedLocation)
      }
    }
    
    
    Seq.fill   (numberOfMines)(0)
       .foreach(x => randomFill(getRandomFlattenedLocation))
  }
  
  
  def IsAnyMineActivated = _fields.exists(row => row.exists(mineSquare => mineSquare.IsActivated))
  
  def MakeMove(move: Move)
  {
    require(!IsAnyMineActivated)
    //If it's Revealed don't do nothing
    if(! (_fields(move.Row)( move.Column).Status == Model.MineSquareStatus.Revealed))
    {
      if(move.MouseClick == Model.MouseClick.Left)
      {
        leftClick(move.Row, move.Column)
      }
      else
      {
        rightClick(move.Row, move.Column)
      }
    }
    
    
    def leftClick(row: Int, column: Int)
    {
      
    }
    def rightClick(row: Int, column: Int)
    {
      _fields(move.Row)( move.Column).Status match
      {
        case status 
            if status == Model.MineSquareStatus.Concealed
                => _fields(move.Row)(move.Column).Status_=(Model.MineSquareStatus.Flagged)
        case status 
            if status == Model.MineSquareStatus.Flagged
                => _fields(move.Row)(move.Column).Status_=(Model.MineSquareStatus.Questioned)
        case status 
            if status == Model.MineSquareStatus.Questioned
                => _fields(move.Row)(move.Column).Status_=(Model.MineSquareStatus.Concealed)
      }
    }
  }
  
  def Print():Unit =
  {
    if (!IsAnyMineActivated) 
    {
      PrintCovered()
    }
    else 
    {
      PrintUncovered() 
    }
  }
  // TODO: Make private
  def PrintCovered  (): Unit = {
    _fields.foreach(
      row => 
        (println(
          row.flatMap(
            _ match {
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Flagged
                      => Model.AsciiMap("Flagged")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Concealed
                      => Model.AsciiMap("Concealed")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Revealed
                      => Model.AsciiMap("Revealed")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Questioned
                      => Model.AsciiMap("Questioned")
            })
            .mkString(" ")
      ))
    )
  }
  def PrintUncovered(): Unit = {
    _fields.foreach(
      row => 
        (println(
          row.flatMap(
            _ match {
              case mineSquare 
                  if mineSquare.HasMine
                      => Model.AsciiMap("Mine")
              case mineSquare 
                  if mineSquare.IsActivated
                      => Model.AsciiMap("Activated")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Concealed
                      => Model.AsciiMap("Concealed")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Revealed
                      => Model.AsciiMap("Revealed")
              case mineSquare 
                  if mineSquare.HasMine && mineSquare.Status == Model.MineSquareStatus.Flagged
                      => Model.AsciiMap("FlaggedAndHasMine")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Flagged
                      => Model.AsciiMap("Flagged")
              case mineSquare
                  if mineSquare.HasMine && mineSquare.Status == Model.MineSquareStatus.Questioned
                      => Model.AsciiMap("QuestionedAndIsMine")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Questioned
                      => Model.AsciiMap("Questioned")
            })
            .mkString(" ")
      ))
    )
  }
}

case class MineSquare
(
  HasMine: Boolean
)
{
  private[this] var _status = Model.MineSquareStatus.Concealed
  def Status = _status
  def Status_= (status: Model.MineSquareStatus.type) { _status = status }
  
  private[this] var _isActivated = false
  def IsActivated = _isActivated
  def IsActivated_= (isActivated: Boolean) { _isActivated = isActivated }
}