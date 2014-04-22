package chess

import scala.collection.mutable.ListBuffer

final class PawnAgent(f: Field, c: Color, override val id: String) extends FigureAgent(f, c, id, 1) {

	val k = {
		color match {
			case White => 1
			case Black => -1
		}
	}

	override def getMoves(game: Game): List[Move] = {		
		val oneAhead = moveDirect(game, (0, k))
		if (oneAhead.isDefined) {
			if (!hasMoved)
				return List(oneAhead, moveDirect(game, (0, 2*k)), moveWithCapture(game, (1, k)), moveWithCapture(game, (-1, k))).flatten
			else
				return List(oneAhead, moveWithCapture(game, (1, k)), moveWithCapture(game, (-1, k))).flatten	
		} else
			List(moveWithCapture(game, (1, k)), moveWithCapture(game, (-1, k))).flatten
	}
	
	// Działa podobnie jak moveDirect, ale zwraca None jeśli pole jest puste
	def moveWithCapture(game: Game, moveCoords: Tuple2[Int, Int]): Option[Move] = {
		val dstField = field.relative(moveCoords._1, moveCoords._2)
		val isInBoard = (dstField.col <= 8 && dstField.row <= 8 && dstField.col >= 1 && dstField.row >= 1)
		if (!isInBoard)
			return None
		val figure: Option[Figure] = game.board.get(dstField)
		figure match {
			case Some(f) if f.color != color => Some(new Move(field, dstField, figureRank(f))) 
			case _ => None
		}
	}

	def hasMoved() = {
		color match {
			case White => field.row != 2
			case Black => field.row != 7
		}
	}
}