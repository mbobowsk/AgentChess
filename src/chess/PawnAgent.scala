package chess

import scala.collection.mutable.ListBuffer

class PawnAgent(override val field: Field, override val color: Color) extends FigureAgent(field, color, 1) {

	val k = {
			color match {
			case White => 1
			case Black => -1
			}
	}

	override def getMoves(game: Game): List[Move] = {
			var acc = List[Move]();
			acc = moveOneAhead(game, acc);
			if (!hasMoved)
				acc = moveTwoAhead(game, acc)

			acc
	}

	def moveOneAhead(game: Game, acc: List[Move]) = {
		val firstAhead: Option[Figure] = game.board.get(field.relative(0, k));
		if (firstAhead == None)
			new Move(field, field.relative(0, k), 0) :: acc
		else
			acc
	}
	
	def moveTwoAhead(game: Game, acc: List[Move]) = {
		val firstAhead: Option[Figure] = game.board.get(field.relative(0, k));
		val secondAhead: Option[Figure] = game.board.get(field.relative(0, 2*k));
		if (firstAhead == None && secondAhead == None)
			new Move(field, field.relative(0, 2*k), 0) :: acc
		else
			acc
	}
	
	def moveCross() = {
		
	}

	def hasMoved() = {
		color match {
		case White => field.row == 1
		case Black => field.row == 7
		}
	}
}