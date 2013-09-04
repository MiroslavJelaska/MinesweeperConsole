package Minesweeper

package object Model
{
  object MineSquareStatus extends Enumeration 
  {
    type MineSquareStatus = Value
    
    val Concealed  = Value("Concealed" )
    val Revealed   = Value("Revealed"  )
    val Flagged    = Value("Flagged"   )
    val Questioned = Value("Questioned")
    val Activated  = Value("Activated" )
  }
  
  object MouseClick extends Enumeration 
  {
    type MouseClick = Value
    
    val Left  = Value("Left" )
    val Right = Value("Right")
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