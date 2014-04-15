package agent
import chess._
import scala.actors.Actor

class PawnAgent(override val field: Field) extends FigureAgent(field, 'p') {
	
	def act {
		println("Start: " + id)
		await
	}
	
	private def await {	  
	  loop {
	    react {
	      case 'allMoves => {
	        println("Got 'allMmoves from superAgent")
	        reply(1)
	      }
	      case _ => println("Unknown msg")
	    }
	  }
	}
}