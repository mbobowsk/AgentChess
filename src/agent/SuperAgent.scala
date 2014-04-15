package agent
import chess._

class SuperAgent(game: Game) {
	var agents = List[FigureAgent]()
	
	for ((field, figure) <- game.board.iterator; if figure.color == game.color) {
	  //agents = new FigureAgent(field, figure) :: agents
	  figure match {
	    case _:Pawn => agents = new PawnAgent(field) :: agents
	    case _:Rook => println("Rook")
	    case _:Knight => println("Knight")
	    case _:Bishop => println("Bishop")
	    case _:Queen => println("Queen")
	    case _:King => println("King")
	    case _ => println("What the fuck?")
	  } 
	}
	agents.foreach(a => println(a.id))
	System.exit(1)
  
}