package chess
import chess._
import akka.actor._
import scala.collection.mutable.Buffer

class SuperAgent(val listener: ActorRef, val color: Color) extends Actor {

	val agents: Buffer[ActorRef] = createFigureAgents();
	var agentsAlive = 2;
	var moves = List[Move]()
	var movesReported = 0;

	def createFigureAgents(): Buffer[ActorRef] = {
		val refs = Buffer[ActorRef]();
		color match {
			case White => {
				refs += context.actorOf(Props(new PawnAgent('a2, color)));
				refs += context.actorOf(Props(new PawnAgent('b2, color)));
				refs += context.actorOf(Props(new PawnAgent('c2, color)));
				refs += context.actorOf(Props(new PawnAgent('d2, color)));
				refs += context.actorOf(Props(new PawnAgent('e2, color)));
				refs += context.actorOf(Props(new PawnAgent('f2, color)));
				refs += context.actorOf(Props(new PawnAgent('g2, color)));
				refs += context.actorOf(Props(new PawnAgent('h2, color)));
			}
			case Black => {
				refs += context.actorOf(Props(new PawnAgent('a7, color)));
				refs += context.actorOf(Props(new PawnAgent('b7, color)));
				refs += context.actorOf(Props(new PawnAgent('c7, color)));
				refs += context.actorOf(Props(new PawnAgent('d7, color)));
				refs += context.actorOf(Props(new PawnAgent('e7, color)));
				refs += context.actorOf(Props(new PawnAgent('f7, color)));
				refs += context.actorOf(Props(new PawnAgent('g7, color)));
				refs += context.actorOf(Props(new PawnAgent('h7, color)));
			}
		}
		return refs;
	}

	def updateState = {
		// TODO - check for kills
		moves = List[Move]()
		movesReported = 0
	}

	def receive = {
		case MakeMove(game: Game) => {
			updateState
			agents.foreach(a => a ! GetMoves(game))
		}
		case FirstMove(move: Move) => {
			agents.foreach(a => a ! Result(move))
		}
		case ReturnMoves(newMoves: List[Move]) => {
			moves = newMoves ::: moves
			movesReported += 1
			if (movesReported == agentsAlive) {
				moves = moves sortBy(_.score)
				println(moves.last.from)
				println(moves.last.to)
				println("===")
				listener ! Result(moves.last)
				agents.foreach(a => a ! Result(moves.last))
			}
		}
	}
}