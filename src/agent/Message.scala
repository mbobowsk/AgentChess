package agent

sealed class Message

case class GetMoves extends Message
case class ReturnMoves(moves: List[Move])