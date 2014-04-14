package chess

import collection.immutable._
import compat.Platform.EOL

object GameRules {
  implicit def game2rules(game: Game): GameRules = new GameRules(game)
}

class GameRules(game: Game) {
  import game.{color, board, previous}, GameRules._, FigureRules._
  def move(from: Field, to: Field, promotion: Option[Figure] = None) =
    validMoves.filter(_.lastMove == (from, to, promotion)).toList.headOption
  def validMoves = nextMoves.filterNot(_.isOtherKingUnderCheck)
  
  def nextMoves = 
    for ((from, figure) <- board.iterator if figure.color == color;
      g <- (figure match {
        case _:Rook | _:Bishop | _:Queen | _:Knight =>
          availableMoves(from.figureMoves(figure)).map(game.updated(from,_))
        case _:King => availableMoves(from.figureMoves(figure)).
            map(game.updated(from,_))++castling(3,1,4,2)++castling(7,8,6,7)
        case p:Pawn =>
          val regularMoves = (movesToFreeFields(from.figureMoves(p,false))++
            movesToOccupiedFields(from.figureMoves(p,true))).
              flatMap(to => if (to.isLastRow(color))
                Seq(Queen(color),Rook(color),Bishop(color),Knight(color)).
                  map(figure => game.updated(from,to,figure))
                else Seq(game.updated(from,to)))
          val enPassantMoves = movesToFreeFields(from.figureMoves(figure,true)).
            filter(isEnPassantCapture(from,_)).map(to =>
              game.updated(from, to, Field(to.col,from.row)))
          regularMoves ++ enPassantMoves
        case _ => Seq.empty }).iterator) yield g
        
  def availableMoves(ss: Set[Seq[Field]]) =
    movesToFreeFields(ss)++movesToOccupiedFields(ss)
  def movesToFreeFields(ss: Set[Seq[Field]]) =
    ss.map(s => s.takeWhile(!board.isDefinedAt(_))).flatten
  def movesToOccupiedFields(ss: Set[Seq[Field]]) =
    ss.map(s => s.dropWhile(!board.isDefinedAt(_)).take(1).
    filter(board(_).color == color.other)).flatten
  
    def castling(kingTo: Int, rookFrom: Int,
               rookTo: Int, otherCol: Int): Seq[OngoingGame] =
    if (board.get(Field(4,firstRow)) == Some(King(color)) &&
        board.get(Field(rookFrom,firstRow)) == Some(Rook(color)) &&
        board.get(Field(rookTo,firstRow)) == None &&
        board.get(Field(kingTo,firstRow)) == None &&
        board.get(Field(otherCol,firstRow)) == None &&
        !game.updated(Field(4,firstRow),Field(rookTo,firstRow)).
          isOtherKingUnderCheck &&
        previous.forall(_.board.get(Field(4,firstRow)) == Some(King(color))) &&
        previous.forall(_.board.get(Field(rookFrom,firstRow)) ==
                        Some(Rook(color))))
      Seq(game.updated(Field(4,firstRow), Field(kingTo,firstRow),
        Field(rookFrom,firstRow), Field(rookTo,firstRow)))
    else Seq()
  def firstRow = Field.firstRow(color)
  
  def isEnPassantCapture(from: Field, to: Field) = game match {
    case GameStart => false
    case g:OngoingGame =>
      g.board.get(g.lastMove._2) != None &&
      g.board(g.lastMove._2).isInstanceOf[Pawn] &&
      g.lastMove._2 == Field(to.col, from.row) &&
      g.lastMove._1 == Field(to.col, from.row + 2*(to.row-from.row))}
  
  def isOtherKingUnderCheck: Boolean = !nextMoves.forall(g => g.board.values.
    exists(fig => fig == King(color.other)))
  def isKingUnderCheck: Boolean = new OngoingGame(color.other,board,
    game :: previous).isOtherKingUnderCheck
  def isGameFinished: Boolean = nextMoves.forall(g => g.isOtherKingUnderCheck) ||
    Set[Set[Figure]](Set(King(White),King(Black)),
                     Set(King(White),King(Black),Bishop(White)),
                     Set(King(White),King(Black),Bishop(Black)),
                     Set(King(White),King(Black),Knight(White)),
                     Set(King(White),King(Black),Knight(Black)),
                     Set(King(White),King(Black),Knight(White),Knight(White)),
                     Set(King(White),King(Black),Knight(Black),Knight(Black))).
      contains(game.board.values.toSet) ||
      !(game.board :: game.previous.map(_.board)).
        groupBy(n=>n).values.toSet.filter(_.size >= 3).isEmpty
  def winner: Option[Color] = if (isGameFinished && game.isKingUnderCheck)
    Some(color.other) else None
}