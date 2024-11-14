package strategygames.chess

import cats.data.Validated.Valid
import strategygames.Player
import strategygames.chess.variant.Marseillais

class MarseillaisVariantTest extends ChessTest {
  "Marseillais chess" should {
    import Pos._
    "This game should end with a win for black" in {
      val game = fenToGame(Marseillais.initialFen, Marseillais)
      game match {
        case Valid(game) => println(game.situation.board.variant.validMoves(game.situation))
        case _           => println("Invalid game")
      }

      val successGame = game.flatMap (_.playMoves(
        G1 -> F3, // 1.
        B1 -> C3, // 1.
        B7 -> B6, // 1...
        G7 -> G6, // 1...
        B2 -> B3, // 2.
        C1 -> B2, // 2.
        C8 -> B7, // 2...
        E7 -> E5, // 2...
        F3 -> E5, // 3.
        E5 -> D3, // 3.
        B7 -> G2, // 3...
        G2 -> H1, // 3...
        F1 -> G2, // 4.
        G2 -> H1, // 4.
        D8 -> G5, // 4...
        G5 -> G1  // 4... Mate
      ))

      successGame must beValid.like {
        case game => game.situation.winner must beSome(beEqualTo(Player.P2))
      }
    }
  }
}
