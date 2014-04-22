package chess
import akka.actor._

abstract class FigureAgent(var field: Field, val color: Color, val id: String, val selfValue: Int) extends Actor {

	final def receive = {
	case GetMoves(game: Game) => {
		sender ! ReturnMoves(getMoves(game))
	}
	case FriendlyMove(move: Move) => {
		if (field == move.from) {
			field = move.to
		}
	}
	case EnemyMove(move: Move) => {
		if (field == move.to) {
			sender ! DeathReport(true, id)
		} else {
			sender ! DeathReport(false, id)
		}
	}
	}

	final def figureRank(figure: Figure) = figure match {
	case _:King => 20
	case _:Queen => 9
	case _:Rook => 5
	case _:Knight | _:Bishop => 3
	case _:Pawn => 1
	case _ => 0 
	}
	
	// Próbuje wykonać ruch bezpośrednio do pola oddalonego o moveCoords
	// Nie bierze pod uwagę zawartości pól pośrednich
	def moveDirect(game: Game, moveCoords: Tuple2[Int, Int]): Option[Move] = {
		val dstField = field.relative(moveCoords._1, moveCoords._2)
		val isInBoard = (dstField.col <= 8 && dstField.row <= 8 && dstField.col >= 1 && dstField.row >= 1)
		if (!isInBoard)
			return None
		val figure: Option[Figure] = game.board.get(dstField)
		figure match {
			case None => Some(new Move(field, dstField))
			case Some(f) if f.color == color => None
			case Some(f) if f.color != color => Some(new Move(field, dstField, figureRank(f))) 
		}
	}
	
	// Zwraca listę wszystkichruchów w pewnym określonym kierunku
	def moveNext(game: Game, direction: Tuple2[Int, Int], iteration: Int = 1): List[Move] = {
		val move = moveDirect(game, (direction._1*iteration, direction._2*iteration))
		move match {
			case None => List()
			case Some(m) if(m.score == 0) => m :: moveNext(game, direction, iteration+1)
			case Some(m) => List(m)
		}
	}

	def getMoves(game: Game) = {
		List[Move]()		
	}

}