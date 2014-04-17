package agent
import chess._
import akka.actor._

abstract class FigureAgent(val field: Field, val id: Char) extends Actor {
	
	def receive = {
	  case GetMoves(game: Game) => {
	    sender ! ReturnMoves(myMoves(game))
	  }
	}
	
	def myMoves(game: Game) = {
	  List[Move]()
	}
}