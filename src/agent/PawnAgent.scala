package agent
import chess._

object PawnAgent {
  private var counter = 1
  private def inc {counter += 1}
}

class PawnAgent(override val field :Field) extends FigureAgent(field) {
	val id:String = "pawn" + PawnAgent.counter;
	PawnAgent.inc;
}