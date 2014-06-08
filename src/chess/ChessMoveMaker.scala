/**
 * Bazuje na fragmencie książki "Język programowania Scala" (Grzegorz Balcerek)
 */
package chess

class MoveMaker(game: Game) {
  import GameRules._
  import scala.util.Random
  import MoveMaker._
  def makeMove = chooseRandomly(moves)
  def chooseRandomly(moves: Seq[Game]) = if (moves.isEmpty) None
    else Some(moves(Random.nextInt(moves.size)))
  def moves = {
    val moves = game.validMoves.toList
    if (moves.isEmpty) Seq()
    else {
      val rankedMoves = moves.map(g => (g, rank(g, game.color)))
      val rankedMovesSorted = rankedMoves.sortBy(- _._2)
      val firstRank = rankedMovesSorted.head._2
      rankedMovesSorted.takeWhile(_._2 == firstRank).map(_._1)}}
}
object MoveMaker {
  import GameRules._
  import FigureRules._
  implicit def game2movemaker(game: Game): MoveMaker = new MoveMaker(game)
  def rank(g: Game, color: Color) = colorRank(g,color)-colorRank(g,color.other)
  def colorRank(game: Game, color: Color) =
    (for ((field, figure) <- game.board.iterator
      if figure.color == color;
      r1 = figureRank(figure);
      r2 = fieldRank(field);
      r3 = figureDefendingOtherFiguresRank(game, field, figure))
    yield r1 + r2 + r3).sum + checkRank(game, color)
  def figureRank(figure: Figure) = figure match {
    case _:Queen => 900
    case _:Rook => 450
    case _:Knight | _:Bishop => 300
    case _:Pawn => 100
    case _ => 0 }
  def fieldRank(field: Field) = 2*colRowRank(field.col) * colRowRank(field.row)
  def colRowRank(cr: Int) = if (cr>=5) 9-cr else cr
  def figureDefendingOtherFiguresRank(game:Game, field:Field, figure:Figure) =
    game.movesToOccupiedFields(field.figureMoves(figure)).size/2
  def checkRank(game: Game, color: Color) =
    if (game.color == color.other && game.isKingUnderCheck) 50
    else 0
}