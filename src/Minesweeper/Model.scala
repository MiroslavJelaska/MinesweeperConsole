package Minesweeper

package object Model
{

  object MineSquareStatus extends Enumeration 
  {
    type MineSquareStatus = Value
    
    val Concealed,
        Revealed,
        Flagged,
        Questioned,
        Activated   = MineSquareStatus
  }
  
  object MouseClick extends Enumeration 
  {
    type MouseClick = Value
    
    val Left,
        Right = MouseClick
  }

  val AsciiMap = Map(
    "Mine"                -> "+",
    "Activated"           -> "*",
    "Concealed"           -> "#",
    "Revealed"            -> ".",
    "FlaggedAndHasMine"   -> "5",
    "Flagged"             -> "4",
    "QuestionedAndIsMine" -> "!",
    "Questioned"          -> "?"
  )
}