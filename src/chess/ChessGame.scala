/**
 * Bazuje na fragmencie książki "Język programowania Scala" (Grzegorz Balcerek)
 */
package chess

import compat.Platform.EOL

abstract sealed class Game {
  type Board = Map[Field,Figure]
  val color: Color
  val previous: List[Game]
  val board: Board
  def rowToString(row: Int) = 1.to(8).map(col=>
    board.get(Field(col,row)).map(_.symbol.toString).getOrElse(".")).mkString
  override def toString = " abcdefgh" + EOL + 8.to(1,-1).map(row =>
    row.toString + rowToString(row) + row.toString + EOL).mkString + " abcdefgh"
  def updated(from:Field, to: Field) = new OngoingGame(color.other,
    board - from + (to->board(from)), this :: previous, (from,to,None))
  def updated(from:Field, to: Field, figure: Figure) =
    new OngoingGame(color.other, board - from + (to->figure),
    this :: previous, (from,to,Some(figure)))
  def updated(from:Field, to: Field, capture: Field) =
    new OngoingGame(color.other, board - from - capture + (to->board(from)),
    this :: previous, (from,to,None))
  def updated(from:Field, to: Field, from2:Field, to2: Field) =
    new OngoingGame(color.other,
    board - from + (to->board(from)) - from2 + (to2->board(from2)),
    this :: previous, (from,to,None))
}

object GameStart extends Game {
  override val color = White
  override val previous: List[Game] = Nil
  override val board = Map[Field,Figure](
    ('a1,Rook(White)),  ('a2,Pawn(White)),('a8,Rook(Black)),  ('a7,Pawn(Black)),
    ('b1,Knight(White)),('b2,Pawn(White)),('b8,Knight(Black)),('b7,Pawn(Black)),
    ('c1,Bishop(White)),('c2,Pawn(White)),('c8,Bishop(Black)),('c7,Pawn(Black)),
    ('d1,Queen(White)), ('d2,Pawn(White)),('d8,Queen(Black)), ('d7,Pawn(Black)),
    ('e1,King(White)),  ('e2,Pawn(White)),('e8,King(Black)),  ('e7,Pawn(Black)),
    ('f1,Bishop(White)),('f2,Pawn(White)),('f8,Bishop(Black)),('f7,Pawn(Black)),
    ('g1,Knight(White)),('g2,Pawn(White)),('g8,Knight(Black)),('g7,Pawn(Black)),
    ('h1,Rook(White)),  ('h2,Pawn(White)),('h8,Rook(Black)),  ('h7,Pawn(Black)))
}

final class OngoingGame(
  override val color: Color,
  override val board: Game#Board,
  override val previous: List[Game],
  val lastMove: (Field, Field, Option[Figure]) = (Field(0,0),Field(0,0),None)
) extends Game {
  override def toString = "Last move: "+color.other+" "+
    lastMove._1+" to "+lastMove._2+EOL+super.toString
}