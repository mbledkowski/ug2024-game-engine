package strategygames.mancala

import com.joansala.game.oware.OwareGame
import com.joansala.game.oware.OwareBoard

import cats.implicits._

import strategygames.{ GameFamily, Player }
import strategygames.mancala.format.{ FEN }
import strategygames.mancala.variant.Variant

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class VariantEnd() extends GameResult
  final case class Draw()       extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int, ended: Boolean): GameResult =
    if (value.abs == 1000) GameResult.VariantEnd()
    else if (value == 0 && ended) GameResult.Draw()
    else if (value == 0 && !ended) GameResult.Ongoing()
    else sys.error(s"Unknown game result: ${value}")

}

object Api {

  abstract class Position {
    val variant: Variant

    def makeMoves(movesList: List[Int]): Position
    def makeMove(move: Int): Position
    def toBoard: String

    def toCoordinates(move: Int): String
    def toNotation (moves: List[Int]): String
    def toMoves(notation: String): List[Int]
    val owareDiagram: String

    val fen: FEN
    //val isImmediateGameEnd: (Boolean, GameResult)
    //val immediateGameEnd: Boolean
    //val optionalGameEnd: Boolean
    //val insufficientMaterial: (Boolean, Boolean)

    //def isDraw(ply: Int): Boolean
    //def hasGameCycle(ply: Int): Boolean
    //val hasRepeated: Boolean

    val pieceMap: PieceMap
    //val piecesInHand: Array[Piece]

    //val optionalGameEndResult: GameResult
    val gameResult: GameResult
    val gameEnd: Boolean
    val legalMoves: Array[Int]
    val playerTurn: Int //1 for South -1 for North
    val getFEN: String
  }

  private class OwarePosition(position: OwareGame) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    val variant = Variant.byKey("oware")

    def makeMoves(movesList: List[Int]): Position = {
      movesList.map { move =>
        if (position.legalMoves.contains(move)) position.makeMove(move)
        else sys.error(s"Illegal move: ${move} from list: ${movesList}")
      }
      new OwarePosition(position)
    }

    def makeMove(move: Int): Position = {
      if (position.legalMoves.contains(move)) position.makeMove(move)
      else sys.error(s"Illegal move: ${move} required one from: ${position.legalMoves}")
      new OwarePosition(position)
    }

    //helper
    def toBoard: String = position.toBoard.toString

    def setBoard(owareBoard: OwareBoard): Unit = position.setBoard(owareBoard)
    def toCoordinates(move: Int): String = position.toBoard().toCoordinates(move)
    def toNotation (moves: List[Int]): String = position.toBoard().toNotation(moves.toArray)

    def toMoves(notation: String): List[Int] = position.toBoard().toMoves(notation).toList

    val owareDiagram: String = position.toBoard().toDiagram()

    private val numHouses = variant.boardSize.width * variant.boardSize.height
    val getFEN: String = 
      owareDiagram
      .split('-')
      .take(numHouses)
      .map{ part => 
        part match {
          case "0" => "1" //empty house
          case _ => 
            part.toIntOption match {
              case Some(x: Int) => 
                x match {
                  case x if x < 27  => (x + 64).toChar
                  case x if x >= 27 => (x + 70).toChar
                  case x if x > 52 => sys.error("expected number of stones less than 53, got " + x.toString())
                }
              case _ => sys.error("expected integer number of string in owareDiagram: " + owareDiagram)
            }
        }
      }
      .mkString("")
      .patch(variant.boardSize.width, "/", 0)
      .concat(" ")
      .concat(owareDiagram.split('-')(numHouses))
      .concat(" ")
      .concat(owareDiagram.split('-')(numHouses + 1))
      .concat(" ")
      .concat(owareDiagram.split('-')(numHouses + 2))
    
    def toPosition = position.toBoard().position()

    lazy val fen: FEN            = FEN(getFEN)

    //this is covered by gameEnd
    //lazy val isImmediateGameEnd: (Boolean, GameResult) = {
    //  val im = position.isImmediateGameEnd()
    //  (im.get0(), GameResult.resultFromInt(im.get1(), givesCheck))
    //}

    //lazy val immediateGameEnd: Boolean = position.hasEnded()

    //dont think there is any optional game end stuff here
    //private lazy val isOptionalGameEnd = position.isOptionalGameEnd()
    //lazy val optionalGameEnd: Boolean  = isOptionalGameEnd.get0()
    //lazy val insufficientMaterial: (Boolean, Boolean) = {
    //  val im = position.hasInsufficientMaterial()
    //  (im.get0(), im.get1())
    //}

    //def isDraw(ply: Int): Boolean       = position.isDraw(ply)
    //def hasGameCycle(ply: Int): Boolean = position.hasGameCycle(ply)
    //lazy val hasRepeated: Boolean       = position.hasRepeated()

    private def pieceFromStonesAndIndex(stones: Int, index: Int): Piece = {
      Piece(if (index < 6) Player.P1 else Player.P2,
            Role.allByMancalaID(GameFamily.Mancala()).get(stones).getOrElse(Role.all(0)) 
      )
    }

    private def convertPieceMapFromFen(fen: String): PieceMap = {
      val pm = scala.collection.mutable.Map[Pos, Piece]()
      val l = FEN(fen).owareStoneArray.zipWithIndex.map{case (seeds, index) => 
        seeds match {
          case 0 => (None, None)
          case n => 
            (
              Pos(index),
              pieceFromStonesAndIndex(n, index)
            )
        }
      }
      .map{ case (Some(pos), Some(piece)) => (pos, piece)}

      l.foreach{case (pos, piece) => pm(pos) -> piece}

      return pm.toMap
    } 
  
    lazy val pieceMap: PieceMap = convertPieceMapFromFen(getFEN)

    //lazy val piecesInHand: Array[Piece] =
    //  vectorOfPiecesToPieceArray(position.piecesInHand(), variant.gameFamily)

    //lazy val optionalGameEndResult: GameResult =
    //  if (isOptionalGameEnd.get0()) GameResult.optionalResultFromInt(isOptionalGameEnd.get1())
    //  else GameResult.Ongoing()

    lazy val gameResult: GameResult =
      GameResult.resultFromInt(position.outcome(), position.hasEnded())

    lazy val gameEnd: Boolean = position.hasEnded()

    val legalMoves: Array[Int] = {
      position.resetCursor()
      var moves: List[Int] = List()
      var nextMove         = position.nextMove()
      while (nextMove != -1) {
        moves = moves ::: List(nextMove)
        nextMove = position.nextMove()
      }
      moves.toArray
    }
    val playerTurn: Int = position.turn()
  }


  def position: Position =
    new OwarePosition(new OwareGame())

 def positionFromVariant(variant: Variant): Position =
   variant.key match {
     case "oware" => new OwarePosition(new OwareGame())
     case _       => new OwarePosition(new OwareGame())
   }

//
//  def positionFromVariantName(variantName: String): Position =
//    new FairyPosition(new FairyStockfish.Position(variantName))
//

  def positionFromVariantNameAndFEN(variantName: String, fen: String): Position = {
      val game = new OwareGame()
      game.setBoard(owareBoardFromFen(fen))
      variantName match {
        case "oware" => new OwarePosition(game)
        case _ => new OwarePosition(new OwareGame())
      }
    }

  def owareBoardFromFen(fen: String): OwareBoard = {
    val myFen = FEN(fen)
    val position: Array[Int] = myFen.owareStoneArray :+ myFen.player1Score :+ myFen.player2Score
    val turn: Int = if(fen.split(" ").last == "S") 1 else -1
    new OwareBoard(position, turn)
  }

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    positionFromVariant(variant).makeMoves(uciMoves.map(m => uciToMove(m)))

  //assumption for uci that 'a1' is bottom left for South player.
  def uciToMove(uciMove: String): Int = {
    uciMove(1).toString() match {
      case "1" => uciMove(0).toInt - 97
      case _   => 11 - ( uciMove(0).toInt - 97 )
    }
  }

  def moveToUci(move: Int): String ={
    move match {
      case x if x < 6 => s"${(x + 97).toChar}1"
      case _          => s"${((11 - move) + 97).toChar}2"
    }
  }

  //  def initialFen(variantName: String): FEN = FEN(FairyStockfish.initialFen(variantName))
  //

   def validateFEN(fen: String): Boolean =
      fen.matches("[A-Za-z0-6]{1,6}/[A-Za-z0-6]{1,6} [\\d]+ [\\d]+ [N|S]")

  //  def positionFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Position =
  //    positionFromVariantNameAndFEN(variantName, fen)
  //      .makeMoves(convertUciMoves(movesList).getOrElse(List.empty))
  //

  def pieceMapFromFen(variantName: String, fen: String): PieceMap = {
    positionFromVariantNameAndFEN(variantName, fen).pieceMap
  }

}