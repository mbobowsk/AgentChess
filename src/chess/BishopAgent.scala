package chess

class BishopAgent (f: Field, c: Color, override val id: String) extends FigureAgent(f, c, id, 3) {
	
	override def getMoves(game: Game): List[Move] = {
		moveNext(game, (1,1)) ++ moveNext(game, (1,-1)) ++ moveNext(game, (-1,1)) ++ moveNext(game, (-1,-1))
	}
	
}