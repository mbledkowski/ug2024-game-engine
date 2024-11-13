package strategygames.chess.variant

import strategygames.chess._

/*
Marseillais Chess (sometimes spelt
incorrectly with a final ‘e’). Origins clouded.
Commonly ascribed to Albert Fortis, a
sometime resident of Marseilles, in
collaboration with another expatriate in the
same city, the Norwegian I. Rossow, c. 1922,
the rules were first published in Le Soleil, a
local newspaper, in 1925. A better claim for
invention would appear to lie with Franzose
Jehan de Queylar who is said to have
formulated the rules sometime during World
War I (Funkschach 1926) but the credit for
popularizing the variant undoubtedly belongs
to Fortis. Each player in turn makes two
consecutive moves either with the same man
or with different men. If check is given on the
first move, this ends the turn. A player must
get out of check on the first move of a turn.
A king cannot move into check and out again.
En passant is legal if the opponent moved a
pawn two squares on either of his moves but
the capture must be made at once. However, if
the opponent made two two-square pawn
moves, both pawns can be taken e.p. This last
rule is credited to Alekhine by F. Palatz in an
article on the subject (L’Echiquier, September
1928). Stalemate can occur if a player can
only make one move but not a second.

See: Page 21, Chapter I in The Classified Encyclopedia of Chess Variants
Link: https://www.jsbeasley.co.uk/encyc/encyc.pdf
*/

case object Marseillais
  extends Variant(
    id = 16,
    key = "marseillais",
    name = "Marseillais",
    standardInitialPosition = true
  ) {
  def perfId: Int    = 24
  def perfIcon: Char = '⁄'

  override def hasFishnet: Boolean = false
  override def exoticChessVariant = true

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)

//  override def lastActionOfTurn(situation: Situation): Boolean = {
//    situation.player match {
//      case P1 => situation.board.lastActionPlayer == Some(P1)
//      case P2 => situation.board.lastActionPlayer == Some(P2)
//    }
//  }
}
