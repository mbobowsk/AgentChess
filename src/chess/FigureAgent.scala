package chess
import akka.actor._

abstract class FigureAgent(val field: Field, val color: Color, val selfValue: Int) extends Actor {
	
	final def receive = {
	  case GetMoves(game: Game) => {
	    sender ! ReturnMoves(getMoves(game))
	  }
	}
	
	def getMoves(game: Game) = {
		List[Move]()		
	}
	
}