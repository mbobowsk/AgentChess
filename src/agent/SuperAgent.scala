package agent
import chess._
import akka.actor._
import scala.collection.mutable.Buffer

class SuperAgent(val listener: ActorRef, val color: Color) extends Actor {

	val agents: Buffer[ActorRef] = createFigureAgents();
	var agentsAlive = 1;
	var moves = List[Move]()
	var movesReported = 0;

	def createFigureAgents(): Buffer[ActorRef] = {
		val refs = Buffer[ActorRef]();
		color match {
			case White => {
				refs += context.actorOf(Props(new PawnAgent('a2)));
			}
			case Black => {
				refs += context.actorOf(Props(new PawnAgent('a7)));
			}
		}
		return refs;
	}

	def updateState = {
		// TODO - check for kills
		moves = List[Move]()
	}

	def receive = {
		case MakeMove(game: Game) => {
			updateState
			agents.foreach(a => a ! GetMoves(game))
		}
		case ReturnMoves(newMoves: List[Move]) => {
			moves = newMoves ::: moves
			movesReported += 1
			if (movesReported == agentsAlive) {
				listener ! Result(moves.head)
			}
		}
	}
}