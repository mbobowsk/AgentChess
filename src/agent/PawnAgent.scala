package agent
import chess._

class PawnAgent(override val field: Field) extends FigureAgent(field, 'p') {

	override def myMoves(game: Game): List[Move] = {
	  val ret = List[Move]()  
		val color = game.color
		if (color == White) {
		  return (new Move(id, field, field.relative(0, 1), 0) :: ret)
		} else {
		  return (new Move(id, field, field.relative(0, -1), 0) :: ret)
		}
	}

}