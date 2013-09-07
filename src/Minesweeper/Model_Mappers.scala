package Minesweeper

package object Mappers
{
    val AsciiMap = scala.collection.Map(
        "Mine"                -> "+",
        "Activated"           -> "*",
        "Concealed"           -> "#",
        "Revealed"            -> ".",
        "FlaggedAndHasMine"   -> "R",
        "Flagged"             -> "P",
        "QuestionedAndIsMine" -> "!",
        "Questioned"          -> "?"
    )
}