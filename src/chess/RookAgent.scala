package chess

class RookAgent (f: Field, c: Color, override val id: String) extends FigureAgent(f, c, id, 5) {
	
	override def getMoves(game: Game): List[Move] = {
		moveNext(game, (0,1)) ++ moveNext(game, (1,0)) ++ moveNext(game, (0,-1)) ++ moveNext(game, (-1,0))
	}
	
	def moveNext(game: Game, direction: Tuple2[Int, Int], iteration: Int = 1): List[Move] = {
		val move = moveDirect(game, (direction._1*iteration, direction._2*iteration))
		move match {
			case None => List()
			case _ => move.get :: moveNext(game, direction, iteration+1)
		}
	}
	
}