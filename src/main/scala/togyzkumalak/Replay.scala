package strategygames.togyzkumalak

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.Player
import strategygames.format.pgn.San
import strategygames.togyzkumalak.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.togyzkumalak.format.{ FEN, Forsyth, Uci }
import strategygames.{ Action => StratAction, Actions, Move => StratMove, Situation => StratSituation }

case class Replay(setup: Game, plies: List[Move], state: Game) {

  lazy val chronoPlies = plies.reverse

  lazy val chronoActions: List[List[Move]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, move) =>
        if (turn.head.head.situationBefore.player != move.situationBefore.player) {
          List(move) +: turn
        } else {
          (turn.head :+ move) +: turn.tail
        }
      }
      .reverse

  def addPly(move: Move) =
    copy(
      plies = move.applyVariantEffect :: plies,
      state = state.apply(move)
    )

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actions: Actions,
      startPlayer: Player,
      activePlayer: Player,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                  = initialFen.getOrElse(variant.initialFen)
    val (init, plies, error) = gameActionWhileValid(actions, startPlayer, activePlayer, fen, variant)
    val game                 = plies.reverse.last._1
    error match {
      case None      => Validated.valid(Reader.Result.Complete(new Replay(init, plies.reverse.map(_._2), game)))
      case Some(msg) => Validated.invalid(msg)
    }
  }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def togyzkumalakMove(action: StratAction) = action match {
    case StratMove.Togyzkumalak(m) => m
    case _                         => sys.error("Invalid togyzkumalak move")
  }

  def replayMove(
      before: Game,
      orig: Pos,
      dest: Pos,
      endTurn: Boolean
  ): Move =
    Move(
      piece = before.situation.board.pieces(orig)._1,
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = before.situation.board.variant.boardAfter(before.situation, orig, dest),
      autoEndTurn = endTurn,
      capture = None,
      promotion = None
    )

  def pliesWithEndTurn(actions: Actions): Seq[(String, Boolean)] =
    actions.zipWithIndex.map { case (a, i) =>
      a.zipWithIndex.map { case (a1, i1) => (a1, i1 == a.size - 1 && i != actions.size - 1) }
    }.flatten

  private def actionsToPliesWithEndTurn(
      actions: Actions,
      startPlayer: Player,
      activePlayer: Player
  ): Seq[(String, Boolean)] =
    pliesWithEndTurn(
      if (Player.fromTurnCount(actions.size + startPlayer.fold(0, 1)) == activePlayer)
        actions :+ Vector()
      else actions
    )

  private def gameActionWhileValid(
      actions: Actions,
      startPlayer: Player,
      activePlayer: Player,
      initialFen: FEN,
      variant: strategygames.togyzkumalak.variant.Variant
  ): (Game, List[(Game, Move)], Option[String]) = {
    val init   = makeGame(variant, initialFen.some)
    var state  = init
    var errors = ""

    def replayMoveFromUci(
        orig: Option[Pos],
        dest: Option[Pos],
        promotion: String,
        endTurn: Boolean
    ): (Game, Move) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          val move = replayMove(state, orig, dest, endTurn)
          state = state(move)
          (state, move)
        }
        case (orig, dest)             => {
          val uciMove = s"${orig}${dest}${promotion}"
          errors += uciMove + ","
          sys.error(s"Invalid move for replay: ${uciMove}")
        }
      }

    val plies: List[(Game, Move)] =
      actionsToPliesWithEndTurn(actions, startPlayer, activePlayer).toList.map {
        case (Uci.Move.moveR(orig, dest, promotion), endTurn) =>
          replayMoveFromUci(
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            promotion,
            endTurn
          )
        case (action: String, _)                              =>
          sys.error(s"Invalid move for replay: $action")
      }

    (init, plies, errors match { case "" => None; case _ => errors.some })
  }

  def gamePlyWhileValid(
      actions: Actions,
      startPlayer: Player,
      activePlayer: Player,
      initialFen: FEN,
      variant: strategygames.togyzkumalak.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, plys, error) = gameActionWhileValid(
      actions,
      startPlayer,
      activePlayer,
      initialFen,
      variant
    )
    (
      game,
      plys.map { v =>
        {
          val (state, move) = v
          (state, Uci.WithSan(move.toUci, "NOSAN"))
        }
      },
      error
    )
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(togyzkumalakMove) flatMap { move =>
          val after = Situation(move.finalizeAfter, !sit.player)
          recursiveSituations(after, rest) map { after :: _ }
        }
    }

  private def recursiveSituationsFromUci(
      sit: Situation,
      ucis: List[Uci]
  ): Validated[String, List[Situation]] =
    ucis match {
      case Nil         => valid(Nil)
      case uci :: rest =>
        uci(sit) andThen { move =>
          val after = Situation(move.finalizeAfter, !sit.player)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { ply =>
          recursiveReplayFromUci(replay.addPly(ply), rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.togyzkumalak.variant.Variant.default)
  } withVariant variant

  def boards(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Board]] = situations(actions, initialFen, variant) map (_ map (_.board))

  def situations(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    // seemingly this isn't used
    Parser.sans(actions.flatten, sit.board.variant) andThen { sans =>
      recursiveSituations(sit, sans.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves) map { sit :: _ }
  }

  private def recursiveGamesFromUci(
      game: Game,
      ucis: List[Uci]
  ): Validated[String, List[Game]] =
    ucis match {
      case Nil                     => valid(List(game))
      case (uci: Uci.Move) :: rest =>
        game.apply(uci) andThen { case (game, _) =>
          recursiveGamesFromUci(game, rest) map { game :: _ }
        }
    }

  def gameFromUciStrings(
      uciStrings: List[String],
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  def apply(
      plies: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), plies)

  def plyAtFen(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant,
      atFen: FEN
  ): Validated[String, Int] =
    if (Forsyth.<<@(variant, atFen).isEmpty) invalid(s"Invalid FEN $atFen")
    else {

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: FEN) = fen.value.split(' ').take(4) mkString " "
      val atFenTruncated        = truncateFen(atFen)
      def compareFen(fen: FEN)  = truncateFen(fen) == atFenTruncated

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int, turn: Int): Validated[String, Int] =
        sans match {
          case Nil         => invalid(s"Can't find $atFenTruncated, reached ply $ply, turn $turn")
          case san :: rest =>
            san(StratSituation.wrap(sit)).map(togyzkumalakMove) flatMap { move =>
              val after        = move.situationAfter
              val newPlies     = ply + 1
              val newTurnCount = turn + (if (sit.player != after.player) 1 else 0)
              val fen          = Forsyth >> Game(after, plies = newPlies, turnCount = newTurnCount)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(after, rest, newPlies, newTurnCount)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      // seemingly this isn't used
      Parser.sans(actions.flatten, sit.board.variant) andThen { sans =>
        recursivePlyAtFen(sit, sans.value, 0, 0)
      }
    }

  private def makeGame(variant: strategygames.togyzkumalak.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(
      startedAtPlies = g.plies,
      startedAtTurn = g.turnCount
    )
  }
}
