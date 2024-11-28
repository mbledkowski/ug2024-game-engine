package strategygames.chess.format
import strategygames.chess._
import strategygames.Player

/** r bqkb r p ppp pp pr P p QnB PP N P PPP RN K R
  */
object Visual {

  def <<(source: String): Board = {
    val lines    = augmentString(source).linesIterator.to(List)
    val filtered = lines.size match {
      case 8          => lines
      case n if n > 8 => lines.slice(1, 9)
      case n          => (List.fill(8 - n)("")) ::: lines
    }
    Board(
      pieces = (for {
        (l, y) <- (filtered zipWithIndex)
        (c, x) <- (l zipWithIndex)
        role   <- Role forsyth c.toLower
      } yield {
        Pos.at(x, 7 - y) map { pos =>
          pos -> (Piece(Player.fromP1(c isUpper), role))
        }
      }) flatten,
      variant = strategygames.chess.variant.Variant.default
    )
  }

  def charToChessPiece(c: Char): String = {
    c match {
      case 'p' => "♟"
      case 'n' => "♞"
      case 'b' => "♝"
      case 'r' => "♜"
      case 'q' => "♛"
      case 'k' => "♚"
      case 'P' => "♙"
      case 'N' => "♘"
      case 'B' => "♗"
      case 'R' => "♖"
      case 'Q' => "♕"
      case 'K' => "♔"
      case x => x.toString
    }
  }

  def >>(board: Board): String = >>|(board, Map.empty)

  def >>|(board: Board, marks: Map[Iterable[Pos], Char]): String = {
    val markedPoss: Map[Pos, Char] = marks.foldLeft(Map[Pos, Char]()) { case (marks, (poss, char)) =>
      marks ++ (poss.toList map { pos =>
        (pos, char)
      })
    }
    for (y <- Rank.allReversed) yield {
      for (x <- File.all) yield {
        val pos = Pos(x, y)
        val char = markedPoss.get(pos) getOrElse board(pos).fold(' ')(_ forsyth)
        if (x == File.A) {
          y.toString + " " + charToChessPiece(char)
        } else {
          " " + charToChessPiece(char)
        }
      }
    } mkString
  }.mkString("\n") + "\n" + (
    for (x <- File.all) yield {
      if (x == File.A) {
        "  " + x.toString + " "
      } else {
        x.toString + " "
      }
    }
  ).mkString

  def addNewLines(str: String) = "\n" + str + "\n"
}
