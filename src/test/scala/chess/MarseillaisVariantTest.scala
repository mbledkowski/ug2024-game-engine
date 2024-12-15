package strategygames.chess

import cats.data.Validated.Valid
import strategygames.Player
import strategygames.chess.format.FEN
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

    "Black to move based on FEN" in {
      val position = FEN("rn1Bk1nr/p2ppp2/8/1p2b1p1/1Pb1P3/2N5/P1PK1PPP/R1Q4R b kq - 0 1")
      val game = fenToGame(position, Marseillais)

      game must beValid.like {
        case game => game.situation.player must beEqualTo(Player.P2)
      }
    }

    "After black makes a check in first move, white's king should be threatened" in {
      val position = FEN("rn1Bk1nr/p2ppp2/8/1p2b1p1/1Pb1P3/2N5/P1PK1PPP/R1Q4R b kq - 0 1")
      val game = fenToGame(position, Marseillais)

      val gameAfterMove = game.flatMap (_.playMoves(
        E5 -> F4
      ))

      gameAfterMove must beValid.like {
        case game =>
          println(s"Game situation: ${game.situation}")
          val board = game.situation.board
          val kingPos =  board.kingPosOf(Player.P1)
          println("kingPos: %s".format(kingPos))
          kingPos exists (x => {
            val isKingThreatened = board.variant.kingThreatened(board, Player.P2, x, _ => true)
            println("isKingThreatened: %s\nPlayer: %s".format(isKingThreatened, Player.P2))
            isKingThreatened
          }) must beTrue
      }

    }

    "After black makes a check in first move, the sides should switch" in {
      val position = FEN("rn1Bk1nr/p2ppp2/8/1p2b1p1/1Pb1P3/2N5/P1PK1PPP/R1Q4R b kq - 0 1")
      val game = fenToGame(position, Marseillais)

      game match {
        case Valid(game) => println("4: %s".format(game.situation.board.variant.validMoves(game.situation)))
        case _           => println("Invalid game")
      }

      val gameAfterMove = game.flatMap (_.playMoves(
        E5 -> F4
      ))

      gameAfterMove match {
        case Valid(game) => println("4b: %s".format(game.situation.board.variant.validMoves(game.situation)))
        case _           => println("Invalid game")
      }

      gameAfterMove must beValid.like { case game =>
        game.situation.moves.values.flatten.size must_== 2
      }

    }
  }
}
