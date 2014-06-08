package chess
import chess._
import akka.actor._
import scala.collection.mutable.Buffer

class SuperAgent(val listener: ActorRef, val color: Color) extends Actor {

	type SavedBoard = scala.collection.mutable.Map[Field, Figure];
	var agentsAlive = 16;
	val agents: scala.collection.mutable.Map[String, ActorRef] = createFigureAgents();
	var beatingMap: scala.collection.mutable.Map[Field, Boolean] = createBeatingDefaultMap();
	var beatenFigure : Option[Figure] = None;
	var movedFigure : Option[Figure] = None;
	var moves = List[Move]();
	var movesReported = 0;
	var deathsReported = 0;
	var someoneDied = false;
	var savedBoard:SavedBoard = scala.collection.mutable.Map[Field, Figure]();
	var savedMove:Move = null;
	var scoresReported = 0
	var totalScore: Double = 0
	var minScore: Double = _
	var firstScoreReported = false
	var friendlyMovesDelivered = 0
	var allAgentsReceivedFriendlyMove = false
	var allAgentsReportedScore = false
	var movesCalculated = 0
	import Action._
	var action: Action = CommonMove
	
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
				refs.put("n1", context.actorOf(Props(new KnightAgent('b1, color, "n1"))));
				refs.put("n2", context.actorOf(Props(new KnightAgent('g1, color, "n2"))));
				refs.put("r1", context.actorOf(Props(new RookAgent('a1, color, "r1"))));
				refs.put("r2", context.actorOf(Props(new RookAgent('h1, color, "r2"))));
				refs.put("b1", context.actorOf(Props(new BishopAgent('c1, color, "b1"))));
				refs.put("b2", context.actorOf(Props(new BishopAgent('f1, color, "b2"))));
				refs.put("q", context.actorOf(Props(new QueenAgent('d1, color, "q"))));
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
				refs.put("n1", context.actorOf(Props(new KnightAgent('b8, color, "n1"))));
				refs.put("n2", context.actorOf(Props(new KnightAgent('g8, color, "n2"))));
				refs.put("r1", context.actorOf(Props(new RookAgent('a8, color, "r1"))));
				refs.put("r2", context.actorOf(Props(new RookAgent('h8, color, "r2"))));
				refs.put("b1", context.actorOf(Props(new BishopAgent('c8, color, "b1"))));
				refs.put("b2", context.actorOf(Props(new BishopAgent('f8, color, "b2"))));
				refs.put("q", context.actorOf(Props(new QueenAgent('d8, color, "q"))));
			}
		}
		agentsAlive = refs.size
		refs
	}

	def createBeatingDefaultMap() : scala.collection.mutable.Map[Field, Boolean] = {
		var temp = scala.collection.mutable.Map[Field, Boolean]()
		var field = Field(1,1)
		for ( i <- Range(0, 8) )
			for( j <- Range(0,8))
				temp.update(field.relative(i, j), false)
		temp
	}
	
	def resetBeatingMap() {
		for (i <- 1 to 9)
			for (j <- 1 to 9)
				beatingMap.update(new Field(i, j), false)
	}

	def updateBeatingMap() {
		resetBeatingMap
		val otherColor = color.other
		printBoard(savedBoard)
		savedBoard foreach {
			case (field, figure) => {
				figure match {
					case King(otherColor) =>
						val movesTemplate = List((0, 1), (0, -1), (1, 0),
							(1, 1), (1, -1), (-1, 0), (-1, 1), (-1, -1))
						movesTemplate.map(saveDirect(field, _))
					case Queen(otherColor) =>
						saveMoves(field, (1, 1))
						saveMoves(field, (1, -1))
						saveMoves(field, (-1, 1))
						saveMoves(field, (-1, -1))
						saveMoves(field, (0, 1))
						saveMoves(field, (1, 0))
						saveMoves(field, (-1, 0))
						saveMoves(field, (0, -1))
					case Rook(otherColor) =>
						saveMoves(field, (0, 1))
						saveMoves(field, (1, 0))
						saveMoves(field, (0, -1))
						saveMoves(field, (-1, 0))
					case Bishop(otherColor) =>
						saveMoves(field, (1, 1))
						saveMoves(field, (1, -1))
						saveMoves(field, (-1, 1))
						saveMoves(field, (-1, -1))
					case Knight(otherColor) =>
						val movesTemplate = List((2, 1), (2, -1), (-2, 1), (-2, -1),
							(1, 2), (1, -2), (-1, 2), (-1, -2))
						movesTemplate.map(saveDirect(field, _))
					case Pawn(otherColor) =>
						//beatingMap(field) = true
						var rowOffset = 1
						if (otherColor == Black) {
							rowOffset = -1
						}
						//Jakiekolwiek bicie
						beatingMap(field.relative(-1, rowOffset)) = true
						beatingMap(field.relative(1, rowOffset)) = true
				}
			}
		}
	}

	def saveMoves(start: Field, direction: Tuple2[Int, Int], iteration: Int = 1) {
		if (saveDirect(start, direction, iteration))
			saveMoves(start, direction, iteration + 1)
	}

	def saveDirect(start: Field, direction: Tuple2[Int, Int], distance: Int = 1) : Boolean = {
		val dstField = start.relative(direction._1*distance, direction._2*distance)
		if(!dstField.isValid) {
			return false
		} else {
			beatingMap.update(dstField, true)
			val figure: Option[Figure] = savedBoard.get(dstField)
			figure match {
				case None => return true
				case Some(f) => return false
			}
		}
	}

	def updateBoard() {
		movedFigure = savedBoard.get(savedMove.from)
		beatenFigure = savedBoard remove savedMove.to
		movedFigure match {
			case Some(f) => savedBoard(savedMove.to) = f
			//Nie powinno sie zdazyc, ale better safe than sorry
			case _ => ;
		}
	}

	def restoreBoard() {
		beatenFigure match {
			case Some(figure) => savedBoard(savedMove.to) = figure
			case _ => ;
		}

		movedFigure match {
			case Some(figure) => savedBoard(savedMove.from) = figure
			case _ => ;
		}
	}

	def updateState = {
		moves = List[Move]()
		movesReported = 0
		deathsReported = 0
		scoresReported = 0
		someoneDied = false
		beatenFigure = None
	}
	
	def clearScoreVariables {
		scoresReported = 0
		totalScore = 0
		firstScoreReported = false
	}

	def receive = {
		case FriendlyMove(move: Move) => {
			action = CommonMove
			agents.foreach(a => a._2 ! FriendlyMove(move))
		}
		case FriendlyMoveAck() => {
			friendlyMovesDelivered += 1
			if (friendlyMovesDelivered == agentsAlive) {
				action match {
					case TemporaryMove => {
						agents.foreach(a => a._2 ! GetScore(savedBoard, beatingMap))
						restoreBoard()
					}
					case RevertTemporaryMove => {
						if (movesCalculated < moves.size) {
							savedMove = moves(movesCalculated)
							action = TemporaryMove
							agents.foreach(a => a._2 ! FriendlyMove(savedMove))
							updateBoard()
							updateBeatingMap()
							printMap(beatingMap)
						} else {
							moves = (moves sortBy (_.score)).reverse
							listener ! Result(moves)
						}
					}
					case _ => ;
				}
				friendlyMovesDelivered = 0
			}
		}
		case EnemyMove(move: Move) => {
			agents.foreach(a => a._2 ! EnemyMove(move))
		}
		case MakeMove(game: Game) => {
			savedBoard = scala.collection.mutable.Map(game.board.toSeq:_*)
			updateState
			agents.foreach(a => a._2 ! GetMoves(game))
		}
		case DeathReport(hasDied: Boolean, id: String) => {
			deathsReported += 1
			if (hasDied) {
				agents.remove(id)
				someoneDied = true
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
				movesCalculated = 0
				if (moves.size > 0) {
					movesCalculated = 0
					savedMove = moves.head
					updateBoard()
					updateBeatingMap()
					action = TemporaryMove
					agents.foreach(a => a._2 ! FriendlyMove(savedMove))
				} else {
					listener ! Result(moves)
				}
			}
		}
		case ReturnScore(score) => {
			if (!firstScoreReported) {
				firstScoreReported = true
				minScore = score
			} else {
				if (score < minScore) minScore = score
			}
			scoresReported += 1
			totalScore += score
			
			if (scoresReported == agentsAlive) {
				totalScore = totalScore/agentsAlive
				moves(movesCalculated).score = moves(movesCalculated).score + minScore + 0.5*totalScore/agentsAlive
				movesCalculated += 1
				action = RevertTemporaryMove
				agents.foreach(a => a._2 ! FriendlyMove(new Move(savedMove.to, savedMove.from)))
				clearScoreVariables
			}
		}
	}
	
	def printMap(map: scala.collection.mutable.Map[Field, Boolean]) {
		for (i <- 1 until 9) {
			for (j <- 1 until 9)
				if (map(new Field(j, i))) print("1 ")
				else print("0 ")
			print("\n")
		}
	}
	
	def printBoard(map: scala.collection.mutable.Map[Field, Figure]) {
		var f: Option[Figure] = None
		for (i <- 1 until 9) {
			for (j <- 1 until 9)
				if ((map.contains(new Field(j, i)))) print(map(new Field(j, i)).symbol + " ")
				else print("  ")
			print("\n")
		}
	}
}

object Action extends Enumeration {
	type Action = Value
	val CommonMove, TemporaryMove, RevertTemporaryMove = Value
}