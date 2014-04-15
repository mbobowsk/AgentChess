package agent
import chess._
import scala.actors.Actor

class SuperAgent(game: Game, val agents: List[FigureAgent]) extends Actor {
	
  def act {
    println("Starting Super-Agent")
    val moves :List[Move] = getMoves(game)
    println("Exit Super-agent")
  }
  
  def getMoves(game: Game): List[Move] = {
    var moves = List[Move]();
    for (a <- agents) {
      println("Sending message")
      a ! 'allMoves
    }
    return moves
  }
  
}