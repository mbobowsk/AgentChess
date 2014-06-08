package chess

class RookAgent (f: Field, c: Color, override val id: String) extends FigureAgent(f, c, id, 5) {
	
	override def getMoves(game: Game): List[Move] = {
		moveNext(game, (0,1)) ++ moveNext(game, (1,0)) ++ moveNext(game, (0,-1)) ++ moveNext(game, (-1,0))
	}
	
}