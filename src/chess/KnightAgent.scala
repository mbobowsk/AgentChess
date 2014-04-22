package chess

class KnightAgent(f: Field, c: Color, override val id: String) extends FigureAgent(f, c, id, 3) {
	
	override def getMoves(game: Game): List[Move] = {
		val movesTemplate = List((2,1), (2,-1), (-2,1), (-2,-1), (1,2), (1,-2), (-1,2), (-1,-2))
		movesTemplate.map(moveDirect(game, _)).flatten
	}
	
}