package agent
import chess._

sealed class Message

case class MakeMove(game: Game) extends Message
case class GetMoves(game: Game) extends Message
case class ReturnMoves(moves: List[Move]) extends Message

// Wynik działania super agenta
case class Result(move: Move)