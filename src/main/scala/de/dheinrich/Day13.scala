package de.dheinrich
import scala.annotation.tailrec

sealed trait Direction {
  def turn(turn: Turn) = (this, turn) match {
    case (North, LeftT)  => West
    case (North, RightT) => East

    case (East, LeftT)  => North
    case (East, RightT) => South

    case (West, LeftT)  => South
    case (West, RightT) => North

    case (South, LeftT)  => East
    case (South, RightT) => West

    case (dir, Straight) => dir
  }

  def move(pos: Vec2) = this match {
    case North => Vec2(pos.x, pos.y - 1)
    case East  => Vec2(pos.x + 1, pos.y)
    case West  => Vec2(pos.x - 1, pos.y)
    case South => Vec2(pos.x, pos.y + 1)
  }

  override def toString: String = getClass.getSimpleName
}

sealed trait Horizontal extends Direction
sealed trait Vertical   extends Direction

object North extends Vertical
object East  extends Horizontal
object West  extends Horizontal
object South extends Vertical

case class Cart(direction: Direction, turnCounter: Int = 0) {
  def turn = (turnCounter % 3) match {
    case 0 => LeftT
    case 1 => Straight
    case 2 => RightT
  }
}

sealed trait Track
object NorthWestCurve                  extends Track
object NorthEastCurve                  extends Track
case class Path(isHorizontal: Boolean) extends Track
object Intersection                    extends Track

sealed trait Turn
object LeftT     extends Turn
object Straight extends Turn
object RightT    extends Turn

object Day13 extends DayApp(13) {
  val parsed = for {
    (line, lineIndex)     <- input.zipWithIndex
    (symbol, symbolIndex) <- line.zipWithIndex
  } yield {
    val coords = Vec2(symbolIndex, lineIndex)
    symbol match {
      case '-'  => (Some(Path(true)), None, coords)
      case '|'  => (Some(Path(false)), None, coords)
      case '\\' => (Some(NorthEastCurve), None, coords)
      case '/'  => (Some(NorthWestCurve), None, coords)
      case '+'  => (Some(Intersection), None, coords)

      case '<' => (Some(Path(true)), Some(Cart(West)), coords)
      case '>' => (Some(Path(true)), Some(Cart(East)), coords)
      case '^' => (Some(Path(false)), Some(Cart(North)), coords)
      case 'v' => (Some(Path(false)), Some(Cart(South)), coords)

      case _ => (None, None, coords)
    }
  }

  val tracks: Map[Vec2, Track] = (for {
    (tOp, _, c) <- parsed
    track       <- tOp
  } yield c -> track).toMap

  type Carts = Seq[(Vec2, Cart)]

  val carts: Seq[(Vec2, Cart)] = for {
    (_, cOp, c) <- parsed
    cart        <- cOp
  } yield c -> cart

  def moveCarts(carts: Carts): Vec2 = {

    @tailrec
    def recMove(toMove: Carts, moved: Carts = Seq()): Either[Vec2, Carts] = {
      val current = toMove.head
      val pos     = current._1
      val track   = tracks(pos)
      val (nextPos, cart) = (track, current._2) match {
        case (Path(_), c @ Cart(dir, _)) => (dir.move(pos), c)

        case (NorthEastCurve, Cart(North, tc)) => (West.move(pos), Cart(West, tc))
        case (NorthEastCurve, Cart(East, tc)) => (South.move(pos), Cart(South, tc))
        case (NorthEastCurve, Cart(West, tc)) => (North.move(pos), Cart(North, tc))
        case (NorthEastCurve, Cart(South, tc)) => (East.move(pos), Cart(East, tc))

        case (NorthWestCurve, Cart(North, tc)) => (East.move(pos), Cart(East, tc))
        case (NorthWestCurve, Cart(East, tc)) => (North.move(pos), Cart(North, tc))
        case (NorthWestCurve, Cart(West, tc)) => (South.move(pos), Cart(South, tc))
        case (NorthWestCurve, Cart(South, tc)) => (West.move(pos), Cart(West, tc))

        case (Intersection, c @ Cart(dir, tc)) => {
          val newDir = dir.turn(c.turn)
          (newDir.move(pos), Cart(newDir, tc + 1))
        }
      }

      if(moved.map(_._1).contains(nextPos) || toMove.map(_._1).contains(nextPos)) {
        Left(nextPos)
      }
      else {
        val nextMoved = (nextPos, cart) +: moved
        if(toMove.tail.isEmpty)
          Right(nextMoved)
        else
          recMove(toMove.tail, nextMoved)
      }
    }

    @tailrec
    def recAllMoves(cts: Carts): Vec2 = {
      implicit val posOrdering: Ordering[Vec2] =
        Ordering.Tuple2[Int, Int].on(v => (v.x, v.y))

      val sorted = cts.sortBy(_._1)
      recMove(sorted) match {
        case Left(v) => v
        case Right(moved) => recAllMoves(moved)
      }
    }

    recAllMoves(carts)
  }

  println(moveCarts(carts))

}
