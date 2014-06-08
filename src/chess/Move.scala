package chess
import chess._

class Move (val from: Field, val to: Field, var score: Double) {
	def this (from: Field, to: Field) = {
		this(from, to, 0)
	}
}