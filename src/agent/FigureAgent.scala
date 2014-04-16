package agent
import chess._
import akka.actor._

abstract class FigureAgent(val field: Field, val id: Char) extends Actor {
	
	def receive = {
	  case GetMoves => {
	    sender ! ReturnMoves(myMoves)
	  }
	}
	
	def myMoves = {
	  List[Move]()
	}
}