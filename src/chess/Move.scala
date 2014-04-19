package chess
import chess._

class Move (val from: Field, val to: Field, var score: Int) {
	def this (from: Field, to: Field) = {
		this(from, to, 0)
	}
}