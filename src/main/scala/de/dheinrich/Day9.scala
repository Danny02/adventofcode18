package de.dheinrich

import fastparse._
import NoWhitespace._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait Circle {
  def normalizeIndex(i: Int) = {
    val mod = i % size
    if(mod < 0) mod + size else mod
  }

  def size: Int
  def apply(i: Int): Int
  def insertAfter(value: Int, after: Int): (Int, Circle)
  def remove(index: Int): Circle

  override def equals(obj: Any): Boolean = obj match {
    case c: Circle => (0 until size).forall(i => apply(i) == c(i))
    case _         => false
  }

  override def toString: String =
    (0 until size).map(apply).mkString("Circle(", ", ", ")")
}

case class MovingCircle(inner: IndexedSeq[Int], offset: Int = 0, shift: Int = 0)
    extends Circle {
  def size = inner.size - offset

  def innerIndex(i: Int) = normalizeIndex(i + shift) + offset

  def clean =
    if(offset >= 4096) MovingCircle(inner.drop(offset), 0, shift) else this

  override def apply(i: Int) = inner(innerIndex(i))

  override def insertAfter(value: Int, after: Int) = {
    val insertAt = normalizeIndex(after + 1)

    if(insertAt == 0)
      (size, MovingCircle(inner :+ value, offset, shift).clean)
    else
      (insertAt,
       MovingCircle(inner ++ inner.take(insertAt) :+ value,
                    offset + insertAt,
                    shift - insertAt - 1).clean)
  }

  override def remove(index: Int) = {
    val n = normalizeIndex(index)

    if(n == 0)
      MovingCircle(inner, offset + 1, shift).clean
    else if(n == size - 1)
      MovingCircle(inner.dropRight(1), offset, shift).clean
    else
      MovingCircle(inner ++ inner.take(n), offset + n + 1, shift - n).clean
  }
}

case class VecCircle(inner: IndexedSeq[Int]) extends Circle {
  override def apply(i: Int) = inner(normalizeIndex(i))
  override def insertAfter(value: Int, after: Int) = {
    val i             = normalizeIndex(after)
    val (front, back) = inner.splitAt(i + 1)
    val nB            = VecCircle(front ++: (value +: back))
    (front.size, nB)
  }
  override def remove(index: Int) = {
    val i = normalizeIndex(index)
    if(i == 0)
      VecCircle(inner.tail)
    else {
      val (front, back) = inner.splitAt(i)
      VecCircle(front ++: back.tail)
    }
  }
  override def size: Int = inner.size
}

object Circle {
  def vec(initial: Int*): Circle = VecCircle(initial.toVector)
  def mov(initial: Int*): Circle = MovingCircle(initial.to[Array])
}

final case class Game(playerCount: Int,
                      maxMarble: Int,
                      highScore: Option[Int]) {
  import Game._

  var time = System.currentTimeMillis()

  @tailrec
  def playMarbles(state: State = State()): Int = {
    import state._
    if(marble % 1000 == 0) {
      val nt = System.currentTimeMillis()
      println(
        s"${marble / 1000}th block took ${nt - time} current size ${state.circle.asInstanceOf[VecCircle].inner.size}")
      time = nt
    }

    if(marble > maxMarble) {
      scores.values.max
    } else if(marble % 23 == 0)
      playMarbles(scoreNextMarble(state))
    else
      playMarbles(addMarble(state))

  }

  def addMarble(state: State) = {
    import state._
    val (ni, nextCircle) = circle.insertAfter(marble, currentIndex + 1)
    State(marble + 1, ni, nextCircle, scores)
  }

  def scoreNextMarble(state: State) = {
    import state._
    val playerId = marble % playerCount

    val toRemove   = circle.normalizeIndex(currentIndex - 7)
    val removed    = circle(toRemove)
    val nextCircle = circle.remove(toRemove)
    val score      = marble + removed

    val updatedScores = scores.updated(playerId, scores(playerId) + score)
    State(marble + 1, toRemove, nextCircle, updatedScores)
  }
}

object Game {
  case class State(marble: Int = 1,
                   currentIndex: Int = 0,
                   circle: Circle = Circle.vec(0),
                   scores: Map[Int, Int] = Map().withDefault(_ => 0))
}

object Day9 extends DayApp(9, true) {

  val games = parseInput(game(_))
  games.foreach(playGame)
//  games.map(g => g.copy(maxMarble = g.maxMarble * 100)).foreach(playGame)

  def game[_: P] =
    P(number ~ " players; last marble is worth " ~ number ~ " points" ~ (": high score is " ~ number).?)
      .map(t => Game(t._1, t._2, t._3))

  def playGame(game: Game) = {

    def normalizeIndex(i: Int, size: Int) = {
      val mod = i % size
      if(mod < 0) mod + size else mod
    }

    def marbleCount(forMarble: Int) = forMarble + 1

    def scoreFor(index22: Int, marble: Int) = {
      val rowSizeWithoutInbetweens = marbleCount(marble - 4)
      val extendedIndex = rowSizeWithoutInbetweens + index22
      val scoreIndex = normalizeIndex(index22 - 7, marbleCount(marble))
      if(scoreIndex < index22){
        val stepsBetweenScoreAndIndex22 = (extendedIndex - scoreIndex) / 2
        marble - stepsBetweenScoreAndIndex22 + 23
      } else {
        (extendedIndex - scoreIndex) * 2
      }
    }



    val emptyScores: Map[Int, Int] = Map().withDefault(_ => 0)
    lazy val gameStream: Stream[(Int, Map[Int, Int])] = ((0, emptyScores)) #:: gameStream.zipWithIndex.map {
      case ((lastIndex, scores), i) => {
        val m = i + 1
        val nSize = m + 1 - (m / 23 * 2)
        if(m % 23 == 0){
          val score = scoreFor(lastIndex, m - 1)
          val playerId = i % game.playerCount
          val nextIndex = normalizeIndex(lastIndex - 7, nSize)
          (nextIndex, scores.updated(playerId, scores(playerId) + score))
        }
        else{
          val nextIndex = normalizeIndex(normalizeIndex(lastIndex + 1, m) + 1, nSize)
          (nextIndex, scores)
        }
      }
    }

    val highScore = gameStream.take(game.maxMarble + 1).last._2.values.max

//    val highScore = game.playMarbles()
    println(s"calculated high score is $highScore")
    game.highScore.foreach(hs => println(s"\tprovided high score is $hs"))
  }
}
