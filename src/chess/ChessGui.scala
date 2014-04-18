package chess

import swing._
import java.awt.{ Rectangle, Font }
import akka.actor._

object PlayWhite extends App(White)
object PlayBlack extends App(Black)
class App(playerColor: Color) extends SimpleSwingApplication {
	def top = new MainFrame {
		title = "Chess game. Player: " + playerColor +
			". Computer: " + playerColor.other + "."
		contents = new GamePanel(playerColor)
	}
}

class GamePanel(playerColor: Color) extends Panel {
	import MoveMaker._, GameRules._

	val system = ActorSystem("TestSystem")
	val listener = system.actorOf(Props(new Listener), name = "Listener")
	val superAgent = system.actorOf(Props(new SuperAgent(listener, playerColor.other)), name = "SuperAgent")

	private[this] var game = if (playerColor == White) GameStart else GameStart.makeMove.get
	private[this] var selectedField: Option[Field] = None
	preferredSize = new Dimension(450, 450)
	def fieldSize = (size.width min size.height) / 9

	if (playerColor == Black) {
		game match {
			case g: OngoingGame => (superAgent ! FirstMove(new Move(g.lastMove._1, g.lastMove._2, 0)))
		}

	}

	listenTo(mouse.clicks)
	reactions += {
		case FieldClicked(field) =>
			if (game.color == playerColor) {
				selectedField match {
					case None =>
						selectedField = Some(field); repaint
					case Some(from) =>
						selectedField = None
						val newGame = game.move(from, field, promotionFigure(from, field))
						if (newGame != None) {
							game = newGame.get
							repaint
							if (game.isGameFinished) gameOver
							else {
								game match {
									case g: OngoingGame => (superAgent ! EnemyMove(new Move(g.lastMove._1, g.lastMove._2, 0)))
								}
								/*val newGame = game.makeMove;
						if (newGame == None) gameOver
						else Swing.onEDT { game = newGame.get; repaint}*/
							}
						}
				}
			}
	}
	object FieldClicked {
		import event._
		def unapply(bc: MouseClicked): Option[Field] = {
			val fs = fieldSize
			val col = (bc.point.x + fs / 2) / fs
			val row = 9 - (bc.point.y + fs / 2) / fs
			val field = new Field(col, row)
			if (field.isValid) Some(field) else None
		}
	}

	def promotionFigure(from: Field, to: Field): Option[Figure] = {
		val promotionFigures = game.validMoves.
			filter(g => g.lastMove._1 == from && g.lastMove._2 == to &&
				g.lastMove._3 != None).map(_.lastMove._3.get).toList
		promotionFigures.size match {
			case 0 => None
			case 1 => Some(promotionFigures.head)
			case _ =>
				Dialog.showInput[Figure](parent = this,
					message = "Choose the figure to promote the Pawn to",
					entries = promotionFigures,
					initial = promotionFigures.head)
		}
	}
	def gameOver {
		deafTo(mouse.clicks)
		Dialog.showMessage(parent = this, message = "Game over. " +
			(game.winner match {
				case Some(winnerColor) => "Winner: " + winnerColor + "."
				case None => "No winner."
			}))
	}

	override def paintComponent(g: Graphics2D) {
		super.paintComponent(g)
		val fs = fieldSize
		drawFields; drawBoardBorder; drawCoordinates;
		drawFigures; drawSelectedField
			def drawFields {
				import java.awt.Color._
				for (col <- 0 to 7; row <- 0 to 7) {
					g.setPaint(if ((col + row) % 2 == 0) YELLOW else RED)
					g.fill(new Rectangle(fs / 2 + col * fs,
						fs / 2 + row * fs, fs, fs))
				}
			}
			def drawBoardBorder {
				g.setPaint(java.awt.Color.BLACK)
				g.draw(new Rectangle(fs / 2, fs / 2, 8 * fs, 8 * fs))
			}
			def drawCoordinates {
				g.setFont(new Font(g.getFont.getName, g.getFont.getStyle, fs * 2 / 5))
				val rect = g.getFont.getStringBounds("0",
					g.getFontRenderContext)
				val xOffset = (rect.getX + rect.getWidth / 2).toInt
				val yOffset = (rect.getY + rect.getHeight / 2).toInt
				for (j <- 1 to 8; row = 9 - j; col = ('a' - 1 + j).toChar) {
					g.drawString(row.toString, fs / 4 - xOffset, j * fs - yOffset)
					g.drawString(row.toString, fs / 4 + fs / 2 + 8 * fs - xOffset, j * fs - yOffset)
					g.drawString(col.toString, j * fs - xOffset, fs / 4 - yOffset)
					g.drawString(col.toString, j * fs - xOffset, fs / 4 + fs / 2 + fs * 8 - yOffset)
				}
			}
			def drawFigures {
				g.setFont(new Font(g.getFont.getName, g.getFont.getStyle, fs))
				val rect = g.getFont.getStringBounds(King(White).u.toString,
					g.getFontRenderContext)
				val xOffset = (rect.getX + rect.getWidth / 2).toInt
				val yOffset = (rect.getY + rect.getHeight / 2).toInt
				for ((field, figure) <- game.board.iterator)
					g.drawString(figure.u.toString, field.col * fs - xOffset,
						(9 - field.row) * fs - yOffset)
			}
			def drawSelectedField {
				selectedField match {
					case Some(field) =>
						g.setPaint(java.awt.Color.BLUE)
						val fs = fieldSize
						g.draw(new Rectangle(-fs / 2 + field.col * fs, -fs / 2 + (9 - field.row) * fs, fs, fs))
						g.draw(new Rectangle(-fs / 2 + field.col * fs + 1, -fs / 2 + (9 - field.row) * fs + 1,
							fs - 2, fs - 2))
					case None =>
				}
			}
	}

	class Listener extends Actor {
		def receive = {
			case EnemyMoveAck => {
				superAgent ! MakeMove(game)
			}
			case Result(move: Move) => {
				val newGame = game.move(move.from, move.to, promotionFigure(move.from, move.to))
				if (newGame == None) { gameOver }
				else Swing.onEDT { game = newGame.get; repaint }
			}
		}
	}

}