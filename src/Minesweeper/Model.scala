package Minesweeper.Model

import Minesweeper.Mappers


sealed trait MouseClick
object MouseClick
{
  case class Left  extends MouseClick 
  case class Right extends MouseClick
}

case class Move
(
  Row:        Int,
  Column:     Int,
  MouseClick: MouseClick
)


class Game
{
    private var _minefield: Option[Minesweeper.Model.Minefield] = None
    
    def StartNewGame(dimensions: (Int, Int), numberOfMines: Int): Unit =
    {
      _minefield = Some(new Minesweeper.Model.Minefield(
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
              case 'L' => MouseClick.Left()
              case 'R' => MouseClick.Right()
            }
      )
      _minefield.get.MakeMove(move)
    }
    
    def Print(): Unit =
    {
      require(!_minefield.isEmpty)
      
      _minefield.get.Print()
    }
    
    //tmp
    def PrintUncovered():Unit = _minefield.get.PrintUncovered
    
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
    Seq.fill   (numberOfMines)(0)
       .foreach(x => randomFill(getRandomFlattenedLocation))
       
       
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
  }
  

  def IsAnyMineActivated = _fields.exists(row => row.exists(mineSquare => mineSquare.IsActivated))
  def MakeMove(move: Move): Unit =
  {
    require(!IsAnyMineActivated)
    require(0 <= move.Row    && move.Row    < numberOfRows    )
    require(0 <= move.Column && move.Column < numberOfColumns )

    if(! (_fields(move.Row)(move.Column).Status == MineSquareStatus.Revealed()))
    {
      move.MouseClick match {
        case MouseClick.Left () => leftClick (move.Row, move.Column)
        case MouseClick.Right() => rightClick(move.Row, move.Column)
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
                  _fields(move.Row)(move.Column).Status_=(MineSquareStatus.Activated())
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
          _fields(last._1)(last._2).Status_=(MineSquareStatus.Revealed())
          
          concealedList = concealedList.init
          
          println(_countSurroundingMines((last._1, last._2)))
          if(_countSurroundingMines((last._1, last._2)) == 0)
          {
            //Up
            if(last._1 != 0)
            {
              if(!_fields(last._1 - 1)(last._2).HasMine &&
                  _fields(last._1 - 1)(last._2).Status == MineSquareStatus.Concealed())
              {
                concealedList ::= (last._1 - 1, last._2)
              }
            }
            // Right
            if(last._2 != (numberOfColumns - 1))
            {
              if(!_fields(last._1)(last._2 + 1).HasMine &&
                  _fields(last._1)(last._2 + 1).Status == MineSquareStatus.Concealed())
              {
                concealedList ::= (last._1, last._2 + 1)
              }
            }
            // Down
            if(last._1 != (numberOfRows - 1))
            {
              if(!_fields(last._1 + 1)(last._2).HasMine &&
                  _fields(last._1 + 1)(last._2).Status == MineSquareStatus.Concealed())
              {
                concealedList ::= (last._1 + 1, last._2)
              }
            }
            // Left
            if(last._2 != 0)
            {
              if(!_fields(last._1)(last._2 - 1).HasMine &&
                  _fields(last._1)(last._2 - 1).Status == MineSquareStatus.Concealed())
              {
                concealedList ::= (last._1, last._2 - 1)
              }
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
            if status == MineSquareStatus.Concealed
                => _fields(move.Row)(move.Column).Status_=(MineSquareStatus.Flagged())
        case status 
            if status == MineSquareStatus.Flagged
                => _fields(move.Row)(move.Column).Status_=(MineSquareStatus.Questioned())
        case status 
            if status == MineSquareStatus.Questioned
                => _fields(move.Row)(move.Column).Status_=(MineSquareStatus.Concealed())
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
  def PrintUncovered(): Unit = _printUncovered //tmp
  
  private def _countSurroundingMines(mineSquareLocation: (Int, Int)): Int =
  {
    var count = 0;
    
    // Up
    if(mineSquareLocation._1 != 0)
    {
      // Up Left
      if(mineSquareLocation._2 != 0)
      {
        if(_fields(mineSquareLocation._1 - 1)(mineSquareLocation._2 - 1).HasMine)
        {
          count = count + 1;
        }
      }
      
      // Up
      if(_fields(mineSquareLocation._1 - 1)(mineSquareLocation._2).HasMine)
      {
        count = count + 1;
      }
      
      // Up Right
      if(mineSquareLocation._2 != (numberOfColumns - 1))
      {
        if(_fields(mineSquareLocation._1 - 1)(mineSquareLocation._2 + 1).HasMine)
        {
          count = count + 1;
        }
      }
    }
    // Right
    if(mineSquareLocation._2 != (numberOfColumns - 1))
    {
      if(_fields(mineSquareLocation._1)(mineSquareLocation._2 + 1).HasMine)
      {
        count = count + 1;
      }
    }
    // Down
    if(mineSquareLocation._1 != (numberOfRows - 1))
    {
      
      // Down Right
      if(mineSquareLocation._2 != (numberOfColumns - 1))
      {
        if(_fields(mineSquareLocation._1 + 1)(mineSquareLocation._2 + 1).HasMine)
        {
          count = count + 1;
        }
      }
      
      // Down
      if(_fields(mineSquareLocation._1 + 1)(mineSquareLocation._2).HasMine)
      {
        count = count + 1;
      }
      
      // Down Left
      if(mineSquareLocation._2 != 0)
      {
        if(_fields(mineSquareLocation._1 + 1)(mineSquareLocation._2 - 1).HasMine)
        {
          count = count + 1;
        }
      }
    }
    // Left
    if(mineSquareLocation._2 != 0)
    {
      if(_fields(mineSquareLocation._1)(mineSquareLocation._2 - 1).HasMine)
      {
        count = count + 1;
      }
    }
    
    count
  }
  private def _printCovered  (): Unit = {
    _fields.foreach(
      row => 
        (println(
          row.flatMap(
            _ match {
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Flagged()
                      => Minesweeper.Mappers.AsciiMap("Flagged")
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Concealed()
                      => Minesweeper.Mappers.AsciiMap("Concealed")
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Revealed()
                      => { println( "(" + _fields.indexOf(row).toString + ", " + row.indexOf(mineSquare).toString + ")" ); "." /*_countSurroundingMines( _fields.indexOf(row), row.indexOf(mineSquare) ).toString*/ }
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Questioned()
                      => Minesweeper.Mappers.AsciiMap("Questioned")
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
                      => Minesweeper.Mappers.AsciiMap("Activated")
              case mineSquare 
                  if mineSquare.HasMine
                      => Minesweeper.Mappers.AsciiMap("Mine")
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Concealed()
                      => Minesweeper.Mappers.AsciiMap("Concealed")
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Revealed()
                      => Minesweeper.Mappers.AsciiMap("Revealed")
              case mineSquare 
                  if mineSquare.HasMine && (mineSquare.Status == MineSquareStatus.Flagged())
                      => Minesweeper.Mappers.AsciiMap("FlaggedAndHasMine")
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Flagged()
                      => Minesweeper.Mappers.AsciiMap("Flagged")
              case mineSquare
                  if mineSquare.HasMine && (mineSquare.Status == MineSquareStatus.Questioned())
                      => Minesweeper.Mappers.AsciiMap("QuestionedAndIsMine")
              case mineSquare
                  if mineSquare.Status == MineSquareStatus.Questioned()
                      => Minesweeper.Mappers.AsciiMap("Questioned")
            })
            .mkString(" ")
      ))
    )
  }
}

sealed trait MineSquareStatus
object MineSquareStatus
{    
    case class Concealed  extends MineSquareStatus
    case class Revealed   extends MineSquareStatus
    case class Flagged    extends MineSquareStatus
    case class Questioned extends MineSquareStatus
    case class Activated  extends MineSquareStatus
}

case class MineSquare
(
  HasMine: Boolean
)
{
  private[this] var _status: MineSquareStatus = MineSquareStatus.Concealed()
  def Status = _status
  def Status_= (status: MineSquareStatus) { _status = status }
  
  private[this] var _isActivated = false
  def IsActivated = _isActivated
  def IsActivated_= (isActivated: Boolean) { _isActivated = isActivated }
}