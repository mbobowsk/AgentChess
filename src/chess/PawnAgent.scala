package chess

import scala.collection.mutable.ListBuffer

final class PawnAgent(f: Field, c: Color) extends FigureAgent(f, c, 1) {

	val k = {
		color match {
			case White => 1
			case Black => -1
		}
	}

	override def getMoves(game: Game): List[Move] = {
		var acc = List[Move]();
		acc = moveOneAhead(game, acc)
		if (!hasMoved)
			acc = moveTwoAhead(game, acc)
		acc = moveCrossLeft(game, acc)
		acc = moveCrossRight(game, acc)
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
		val secondAhead: Option[Figure] = game.board.get(field.relative(0, 2 * k));
		if (firstAhead == None && secondAhead == None)
			new Move(field, field.relative(0, 2 * k), 0) :: acc
		else
			acc
	}

	def moveCrossLeft(game: Game, acc: List[Move]) = {
		if (field.col != 1) {
			val cross: Option[Figure] = game.board.get(field.relative(-1, k));
			cross match {
				case Some(figure) => {
					if (figure.color != color)
						new Move(field, field.relative(-1, k), figureRank(figure)) :: acc
					else
						acc
				}
				case None => acc
			}
		} else
			acc
	}

	def moveCrossRight(game: Game, acc: List[Move]) = {
		if (field.col != 8) {
			val cross: Option[Figure] = game.board.get(field.relative(1, k));
			cross match {
				case Some(figure) => {
					if (figure.color != color)
						new Move(field, field.relative(1, k), figureRank(figure)) :: acc
					else
						acc
				}
				case None => acc
			}
		} else
			acc
	}

	def hasMoved() = {
		color match {
			case White => field.row != 2
			case Black => field.row != 7
		}
	}
}