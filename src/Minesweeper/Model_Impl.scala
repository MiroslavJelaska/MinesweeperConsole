package Minesweeper.Model.Impl

import Minesweeper.Model

case class Move
(
  Row:        Int,
  Column:     Int,
  MouseClick: Model.MouseClick.Value
)


class Game
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
    require(0 <= move.Row    && move.Row    < numberOfRows    )
    require(0 <= move.Column && move.Column < numberOfColumns )

    if(! (_fields(move.Row)(move.Column).Status == Model.MineSquareStatus.Revealed))
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
      _fields(move.Row)( move.Column) match
      {
        case mineSquare 
            if mineSquare.HasMine
                => {
                  _fields(move.Row)(move.Column).IsActivated_=(true)
                  _fields(move.Row)(move.Column).Status_=(Model.MineSquareStatus.Activated)
                }
        case mineSquare
            if !mineSquare.HasMine
                => floodFillForRevealingSquares(
                      startingSquareLocation = (move.Row, move.Column)
                   )
      }
      
      def floodFillForRevealingSquares(startingSquareLocation: (Int, Int))
      {
        var concealedList = (new Tuple2(startingSquareLocation._1, startingSquareLocation._2)) :: Nil
        
        while(!concealedList.isEmpty)
        {
          val last = concealedList.last
          _fields(last._1)(last._2).Status_=(Model.MineSquareStatus.Revealed)
          
          concealedList = concealedList.init
          
          // Up
          if(last._1 != 0)
          {
            if(!_fields(last._1 - 1)(last._2).HasMine &&
                _fields(last._1 - 1)(last._2).Status == Model.MineSquareStatus.Concealed)
            {
              concealedList ::= (last._1 - 1, last._2)
            }
          }
          // Right
          if(last._2 != (numberOfColumns - 1))
          {
            if(!_fields(last._1)(last._2 + 1).HasMine &&
                _fields(last._1)(last._2 + 1).Status == Model.MineSquareStatus.Concealed)
            {
              concealedList ::= (last._1, last._2 + 1)
            }
          }
          // Down
          if(last._1 != (numberOfRows - 1))
          {
            if(!_fields(last._1 + 1)(last._2).HasMine &&
                _fields(last._1 + 1)(last._2).Status == Model.MineSquareStatus.Concealed)
            {
              concealedList ::= (last._1 + 1, last._2)
            }
          }
          // Left
          if(last._2 != 0)
          {
            if(!_fields(last._1)(last._2 - 1).HasMine &&
                _fields(last._1)(last._2 - 1).Status == Model.MineSquareStatus.Concealed)
            {
              concealedList ::= (last._1, last._2 - 1)
            }
          }
        }
      }
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
      _printCovered()
    }
    else 
    {
      _printUncovered() 
    }
  }
  
  private def _printCovered  (): Unit = {
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
  private def _printUncovered(): Unit = {
    _fields.foreach(
      row => 
        (println(
          row.flatMap(
            _ match {
              case mineSquare 
                  if mineSquare.IsActivated
                      => Model.AsciiMap("Activated")
              case mineSquare 
                  if mineSquare.HasMine
                      => Model.AsciiMap("Mine")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Concealed
                      => Model.AsciiMap("Concealed")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Revealed
                      => Model.AsciiMap("Revealed")
              case mineSquare 
                  if mineSquare.HasMine && (mineSquare.Status == Model.MineSquareStatus.Flagged)
                      => Model.AsciiMap("FlaggedAndHasMine")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Flagged
                      => Model.AsciiMap("Flagged")
              case mineSquare
                  if mineSquare.HasMine && (mineSquare.Status == Model.MineSquareStatus.Questioned)
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
  def Status_= (status: Model.MineSquareStatus.Value) { _status = status }
  
  private[this] var _isActivated = false
  def IsActivated = _isActivated
  def IsActivated_= (isActivated: Boolean) { _isActivated = isActivated }
}