package chess
import collection.immutable._

object FigureRules {
	implicit def field2moves(field: Field):FigureRules = new FigureRules(field)
}

class FigureRules(field: Field) {
	def figureMoves(figure: Figure, capture: Boolean = true) =
		figure match {
			case _:Rook => rookMoves
			case _:Bishop => bishopMoves
			case _:Queen => rookMoves++bishopMoves
			case _:King => (rookMoves++bishopMoves).map(_ take 1)
			case _:Knight => Set((1,2),(2,1),(-1,2),(2,-1),(-1,-2),(-2,-1),(1,-2),
					(-2,1)).map(d=>field.relative(d._1,d._2)).filter(_.isValid).map(Seq(_))
			case p:Pawn => capture match {
				case false => (p.color, field.row) match {
					case (White, 2) => Set(Seq(field.relative(0,1),field.relative(0,2)))
					case (White, _) => Set(Seq(field.relative(0,1)))
					case (Black, 7) => Set(Seq(field.relative(0,-1),field.relative(0,-2)))
					case (Black, _) => Set(Seq(field.relative(0,-1))) 
				}
				case true => p.color match {
					case White => Set(Seq(field.relative(-1,1),field.relative(1,1)))
					case Black => Set(Seq(field.relative(-1,-1),field.relative(1,-1))) 
				}
			}
	}
	private def rookMoves = Set(next(0,1),next(0,-1), next(1,0),next(-1,0))
	private def bishopMoves = Set(next(1,1),next(1,-1), next(-1,1),next(-1,-1))
	private implicit val emptyNextFields: Seq[Field] = Seq[Field]()
	private def next(c:Int, r:Int)(implicit fieldSeq:Seq[Field]):Seq[Field] = {
		val nextField = field.relative(c,r)
				if(nextField.isValid)
					new FigureRules(nextField).next(c,r)(fieldSeq ++ Seq(nextField))
				else fieldSeq 
	}
}