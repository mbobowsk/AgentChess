package agent
import chess._
import akka.actor._

class SuperAgent(game: Game, val agents: List[FigureAgent]) extends Actor {
  
  def receive = {
    case _ => print("Siema")
  } 
}