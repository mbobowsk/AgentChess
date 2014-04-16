/**
 * @author Michał Bobowski
 * W proxy tworzymy agentów, uruchamiamy ich działanie oraz tłumaczymy wyniki algorytmu na język gry.
 */
package agent
import chess._
import akka.actor._

class Proxy(game: Game) {
  
	val system = ActorSystem("TestSystem")
  
	//val agents = createFigureAgents(game);
	//agents.foreach(a => println(a.id))

	//val superAgent :SuperAgent = new SuperAgent(game, agents)


	def createFigureAgents(game: Game): List[FigureAgent] = {
			var agents = List[FigureAgent]();

			for ((field, figure) <- game.board.iterator; if figure.color == game.color) {
				figure match {
				//case _:Pawn => agents = new PawnAgent(field) :: agents
				case _:Pawn => println("Pawn")
				case _:Rook => println("Rook")
				case _:Knight => println("Knight")
				case _:Bishop => println("Bishop")
				case _:Queen => println("Queen")
				case _:King => println("King")
				case _ => println("What the fuck?")
				}
			}
			return agents
	}

	def makeMove {
		
	}
}