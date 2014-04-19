package chess
import chess._
import akka.actor._
import scala.collection.mutable.Buffer

class SuperAgent(val listener: ActorRef, val color: Color) extends Actor {

	val agents: scala.collection.mutable.Map[String, ActorRef] = createFigureAgents();
	var agentsAlive = 9;
	var moves = List[Move]()
	var movesReported = 0;
	var deathsReported = 0;
	var someoneDied = false;

	def createFigureAgents() = {
		val refs = scala.collection.mutable.Map[String, ActorRef]();
		color match {
			case White => {
				refs.put("p1", context.actorOf(Props(new PawnAgent('a2, color, "p1"))));
				refs.put("p2", context.actorOf(Props(new PawnAgent('b2, color, "p2"))));
				refs.put("p3", context.actorOf(Props(new PawnAgent('c2, color, "p3"))));
				refs.put("p4", context.actorOf(Props(new PawnAgent('d2, color, "p4"))));
				refs.put("p5", context.actorOf(Props(new PawnAgent('e2, color, "p5"))));
				refs.put("p6", context.actorOf(Props(new PawnAgent('f2, color, "p6"))));
				refs.put("p7", context.actorOf(Props(new PawnAgent('g2, color, "p7"))));
				refs.put("p8", context.actorOf(Props(new PawnAgent('h2, color, "p8"))));
				refs.put("k", context.actorOf(Props(new KingAgent('e1, color, "k"))));
			}
			case Black => {
				refs.put("p1", context.actorOf(Props(new PawnAgent('a7, color, "p1"))));
				refs.put("p2", context.actorOf(Props(new PawnAgent('b7, color, "p2"))));
				refs.put("p3", context.actorOf(Props(new PawnAgent('c7, color, "p3"))));
				refs.put("p4", context.actorOf(Props(new PawnAgent('d7, color, "p4"))));
				refs.put("p5", context.actorOf(Props(new PawnAgent('e7, color, "p5"))));
				refs.put("p6", context.actorOf(Props(new PawnAgent('f7, color, "p6"))));
				refs.put("p7", context.actorOf(Props(new PawnAgent('g7, color, "p7"))));
				refs.put("p8", context.actorOf(Props(new PawnAgent('h7, color, "p8"))));
				refs.put("k", context.actorOf(Props(new KingAgent('e8, color, "k"))));
			}
		}
		refs
	}

	def updateState = {
		moves = List[Move]()
		movesReported = 0
		deathsReported = 0
		someoneDied = false
	}

	def receive = {
		case FriendlyMove(move: Move) => {
			agents.foreach(a => a._2 ! FriendlyMove(move))
		}
		case EnemyMove(move: Move) => {
			agents.foreach(a => a._2 ! EnemyMove(move))
		}
		case MakeMove(game: Game) => {
			updateState
			agents.foreach(a => a._2 ! GetMoves(game))
		}
		case DeathReport(hasDied: Boolean, id: String) => {
			deathsReported += 1
			if (hasDied) {
				print("Died: " + id)
				agents.remove(id)
				someoneDied = true
				if (id == "k") {
					listener ! GameOver(color.other)
					context.stop(self)
				}
			}
			if (deathsReported == agentsAlive) {
				if (someoneDied) {
					agentsAlive -= 1
				}
				listener ! EnemyMoveAck
			}
		}

		case ReturnMoves(newMoves: List[Move]) => {
			moves = newMoves ::: moves
			movesReported += 1
			if (movesReported == agentsAlive) {
				moves = moves sortBy (_.score)
				listener ! Result(moves.last)
				agents.foreach(a => a._2 ! FriendlyMove(moves.last))
			}
		}
	}
}