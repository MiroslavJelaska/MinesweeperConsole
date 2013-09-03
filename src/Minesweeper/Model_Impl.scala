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
  
  def PrintCovered(): Unit = {
    _fields.foreach(
      row => 
        (println(
          row.flatMap(
            _ match {
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Concealed
                      => Model.AsciiMap("Concealed")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Revealed
                      => Model.AsciiMap("Revealed")
              case mineSquare
                  if mineSquare.Status == Model.MineSquareStatus.Flagged
                      => Model.AsciiMap("Flagged")
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
  
  private[this] var _isActivated =  false
  def IsActivated = _isActivated
  def IsActivated_= (isActivated: Boolean) { _isActivated = isActivated }
}