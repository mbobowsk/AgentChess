package chess
import chess._

sealed class Message

// Komunikat uruchamiający pracę super agenta
case class MakeMove(game: Game) extends Message
// Faza 1 - ustalenie ruchów
case class GetMoves(game: Game) extends Message
case class ReturnMoves(moves: List[Move]) extends Message

// Wynik działania super agenta
case class Result(move: Move)