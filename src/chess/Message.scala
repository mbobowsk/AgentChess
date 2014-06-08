package chess
import chess._
import scala.collection.mutable.Map

sealed class Message

// Komunikat uruchamiający pracę super agenta
case class MakeMove(game: Game) extends Message

// Informacje ruchach
case class EnemyMove(move: Move) extends Message
case class FriendlyMove(move: Move) extends Message
case class FriendlyMoveAck() extends Message

// Odpowiedź agenta na informację o ruchu przeciwnika
// Wysyłana również wtedy, kiedy agent nie umarł
case class DeathReport(hasDied: Boolean, id: String) extends Message

// Potwierdzenie uaktualnienia stanu agentów, wysyłane przez super agenta
case class EnemyMoveAck extends Message

// Faza 1 - ustalenie ruchów
case class GetMoves(game: Game) extends Message
case class ReturnMoves(moves: List[Move]) extends Message

// Faza 2 - ocena ruchu
case class GetScore(board: collection.mutable.Map[Field, Figure],
		beating: Map[Field, Boolean])
case class ReturnScore(score: Int)

// Wynik działania super agenta
case class Result(move: List[Move]) extends Message

