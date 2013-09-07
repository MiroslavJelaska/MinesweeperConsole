package Minesweeper.Model

import Minesweeper.Mappers

case class Dimension
(
    numberOfRows:    Int,
    numberOfColumns: Int
)

case class Location
(
    Row:    Int,
    Column: Int
)


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
    
    def StartNewGame(dimensions: Dimension, numberOfMines: Int): Unit =
    {
        _minefield = Some(new Minesweeper.Model.Minefield(
          numberOfRows    = dimensions.numberOfRows,
          numberOfColumns = dimensions.numberOfColumns,
          numberOfMines   = numberOfMines
        ))
    }
    
    def MakeMove(location: Location, click: Char) =
    {
        require(!_minefield.isEmpty)
        require(click == 'L' || click == 'R')
        
        val move =  new Move(
            Row        = location.Row,
            Column     = location.Column,
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
    
    private val _fields = Array.fill(numberOfRows, numberOfColumns)(MineSquare.MineSquare(false))
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
              _fields(flatenedLocation / numberOfRows)(flatenedLocation % numberOfRows) = MineSquare.MineSquare(true)
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
        
        if(! (_fields(move.Row)(move.Column).Status == MineSquare.Status.Revealed()))
        {
            move.MouseClick match {
                case MouseClick.Left () => leftClick (Location(move.Row, move.Column))
                case MouseClick.Right() => rightClick(Location(move.Row, move.Column))
            }
        }
        
        
        def leftClick(location: Location)
        {
            _fields(location.Row)( location.Column) match
            {
                case mineSquare 
                    if mineSquare.HasMine
                        => {
                          _fields(location.Row)(location.Column).IsActivated_=(true)
                          _fields(location.Row)(location.Column).Status_=(MineSquare.Status.Activated())
                        }
                case mineSquare
                    if !mineSquare.HasMine
                        => floodFillForRevealingSquares(
                              startingSquareLocation = Location(location.Row, location.Column)
                           )
            }
            
            
            def floodFillForRevealingSquares(startingSquareLocation: Location)
            {
                var concealedList = startingSquareLocation :: Nil
                
                while(!concealedList.isEmpty)
                {
                    val last = concealedList.last
                    _fields(last.Row)(last.Column).Status_=(MineSquare.Status.Revealed())
                    
                    concealedList = concealedList.init
                    
                    if(_countSurroundingMines(Location(last.Row, last.Column)) == 0)
                    {
                        //Up
                        if(last.Row != 0)
                        {
                            if(!_fields(last.Row - 1)(last.Column).HasMine &&
                                _fields(last.Row - 1)(last.Column).Status == MineSquare.Status.Concealed())
                            {
                                concealedList ::= Location(last.Row - 1, last.Column)
                            }
                        }
                        // Right
                        if(last.Column != (numberOfColumns - 1))
                        {
                            if(!_fields(last.Row)(last.Column + 1).HasMine &&
                                _fields(last.Row)(last.Column + 1).Status == MineSquare.Status.Concealed())
                            {
                                concealedList ::= Location(last.Row, last.Column + 1)
                            }
                        }
                        // Down
                        if(last.Row != (numberOfRows - 1))
                        {
                            if(!_fields(last.Row + 1)(last.Column).HasMine &&
                                _fields(last.Row + 1)(last.Column).Status == MineSquare.Status.Concealed())
                            {
                                concealedList ::= Location(last.Row + 1, last.Column)
                            }
                        }
                        // Left
                        if(last.Column != 0)
                        {
                            if(!_fields(last.Row)(last.Column - 1).HasMine &&
                                _fields(last.Row)(last.Column - 1).Status == MineSquare.Status.Concealed())
                            {
                                concealedList ::= Location(last.Row, last.Column - 1)
                            }
                        }
                    }
                }
            }
        }
        def rightClick(location: Location)
        {
            _fields(location.Row)( location.Column).Status match
            {
                case status 
                    if status == MineSquare.Status.Concealed
                        => _fields(location.Row)(location.Column).Status_=(MineSquare.Status.Flagged())
                case status 
                    if status == MineSquare.Status.Flagged
                        => _fields(location.Row)(location.Column).Status_=(MineSquare.Status.Questioned())
                case status 
                    if status == MineSquare.Status.Questioned
                        => _fields(location.Row)(location.Column).Status_=(MineSquare.Status.Concealed())
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
    
    private def _countSurroundingMines(mineSquareLocation: Location): Int =
    {
        var count = 0;
        
        // Up
        if(mineSquareLocation.Row != 0)
        {
            // Up Left
            if(mineSquareLocation.Column != 0)
            {
                if(_fields(mineSquareLocation.Row - 1)(mineSquareLocation.Column - 1).HasMine)
                {
                    count = count + 1;
                }
            }
            
            // Up
            if(_fields(mineSquareLocation.Row - 1)(mineSquareLocation.Column).HasMine)
            {
                count = count + 1;
            }
            
            // Up Right
            if(mineSquareLocation.Column != (numberOfColumns - 1))
            {
                if(_fields(mineSquareLocation.Row - 1)(mineSquareLocation.Column + 1).HasMine)
                {
                    count = count + 1;
                }
            }
        }
        // Right
        if(mineSquareLocation.Column != (numberOfColumns - 1))
        {
            if(_fields(mineSquareLocation.Row)(mineSquareLocation.Column + 1).HasMine)
            {
                count = count + 1;
            }
        }
        // Down
        if(mineSquareLocation.Row != (numberOfRows - 1))
        {
            
            // Down Right
            if(mineSquareLocation.Column != (numberOfColumns - 1))
            {
                if(_fields(mineSquareLocation.Row + 1)(mineSquareLocation.Column + 1).HasMine)
                {
                    count = count + 1;
                }
            }
            
            // Down
            if(_fields(mineSquareLocation.Row + 1)(mineSquareLocation.Column).HasMine)
            {
                count = count + 1;
            }
            
            // Down Left
            if(mineSquareLocation.Column != 0)
            {
                if(_fields(mineSquareLocation.Row + 1)(mineSquareLocation.Column - 1).HasMine)
                {
                    count = count + 1;
                }
            }
        }
        // Left
        if(mineSquareLocation.Column != 0)
        {
            if(_fields(mineSquareLocation.Row)(mineSquareLocation.Column - 1).HasMine)
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
                                if mineSquare.Status == MineSquare.Status.Flagged()
                                    => Minesweeper.Mappers.AsciiMap("Flagged")
                            case mineSquare
                                if mineSquare.Status == MineSquare.Status.Concealed()
                                    => Minesweeper.Mappers.AsciiMap("Concealed")
                            case mineSquare
                                if mineSquare.Status == MineSquare.Status.Revealed()
                                    => { println( "(" + _fields.indexOf(row).toString + ", " + row.indexOf(mineSquare).toString + ")" ); "." /*_countSurroundingMines( _fields.indexOf(row), row.indexOf(mineSquare) ).toString*/ }
                            case mineSquare
                                if mineSquare.Status == MineSquare.Status.Questioned()
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
                                if mineSquare.Status == MineSquare.Status.Concealed()
                                    => Minesweeper.Mappers.AsciiMap("Concealed")
                            case mineSquare
                                if mineSquare.Status == MineSquare.Status.Revealed()
                                    => Minesweeper.Mappers.AsciiMap("Revealed")
                            case mineSquare 
                                if mineSquare.HasMine && (mineSquare.Status == MineSquare.Status.Flagged())
                                    => Minesweeper.Mappers.AsciiMap("FlaggedAndHasMine")
                            case mineSquare
                                if mineSquare.Status == MineSquare.Status.Flagged()
                                    => Minesweeper.Mappers.AsciiMap("Flagged")
                            case mineSquare
                                if mineSquare.HasMine && (mineSquare.Status == MineSquare.Status.Questioned())
                                    => Minesweeper.Mappers.AsciiMap("QuestionedAndIsMine")
                            case mineSquare
                                if mineSquare.Status == MineSquare.Status.Questioned()
                                    => Minesweeper.Mappers.AsciiMap("Questioned")
                        })
                        .mkString(" ")
            ))
        )
    }
}


object MineSquare
{
    sealed trait Status
    object Status
    {    
        case class Concealed  extends Status
        case class Revealed   extends Status
        case class Flagged    extends Status
        case class Questioned extends Status
        case class Activated  extends Status
    }
    
    case class MineSquare
    (
        HasMine: Boolean
    )
    {
        private[this] var _status: Status = Minesweeper.Model.MineSquare.Status.Concealed()
        def Status = _status
        def Status_= (status: Status) { _status = status }
        
        private[this] var _isActivated = false
        def IsActivated = _isActivated
        def IsActivated_= (isActivated: Boolean) { _isActivated = isActivated }
    }
}