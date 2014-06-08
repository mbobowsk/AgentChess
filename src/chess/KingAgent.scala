package chess

final class KingAgent(f: Field, c: Color, override val id: String) extends FigureAgent(f, c, id, 20) {
	
	override def getMoves(game: Game): List[Move] = {
		val movesTemplate = List((0,1), (0,-1), (1,0), (1,1), (1,-1), (-1,0), (-1,1), (-1,-1))
		movesTemplate.map(moveDirect(game, _)).flatten
	}
	
}