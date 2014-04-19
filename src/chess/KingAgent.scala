package chess

final class KingAgent(f: Field, c: Color, override val id: String) extends FigureAgent(f, c, id, 20) {
	
	override def getMoves(game: Game): List[Move] = {
		// TODO - ruchy króla
		List[Move]()
	}
	
}