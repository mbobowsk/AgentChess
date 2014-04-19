package chess
import akka.actor._

abstract class FigureAgent(var field: Field, val color: Color, val id: String, val selfValue: Int) extends Actor {

	final def receive = {
	case GetMoves(game: Game) => {
		sender ! ReturnMoves(getMoves(game))
	}
	case Result(move: Move) => {
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

	def getMoves(game: Game) = {
		List[Move]()		
	}


}