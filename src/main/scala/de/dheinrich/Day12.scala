package de.dheinrich

import fastparse.NoWhitespace._
import fastparse._

import scala.annotation.tailrec
import scala.collection.BitSet

trait Pot
object Plant    extends Pot {
  override def toString: String = "P"
}
object EmptyPot extends Pot{
  override def toString: String = "E"
}

object Day12 extends DayApp(12) {

  type State = (Pot, Int)

  val state0: Seq[State] = parseLine(initialState(_), input.head).zipWithIndex
  val rules : Map[Seq[Pot], Pot]             = input.drop(2).map(s => parseLine(rule(_), s)).filter(_._2 != EmptyPot).toMap.withDefault(_ => EmptyPot)

  BitSet.empty.

  def nextState(state: Seq[State]): Seq[State] = {
    val first = state.head._2
    val last  = state.last._2
    val world = Seq((EmptyPot, first - 3), (EmptyPot, first - 2), (EmptyPot, first - 1)) ++
      state ++
      Seq((EmptyPot, last + 1), (EmptyPot, last + 2), (EmptyPot, last + 3))

    val ns = world.sliding(5).map(a => (rules(a.map(_._1)), a(2)._2)).toList
    ns.dropWhile(_._1 == EmptyPot).reverse.dropWhile(_._1 == EmptyPot).reverse
  }

  @tailrec
  def sumOf(n: Long, state: Seq[State]): Int = {
    if(n % 1000000000 == 0){
      println(n)
    }

    if(n == 0)
      state.filter(_._1 == Plant).map(_._2).sum
    else
      sumOf(n -1, (state))
  }

  println(s"after 20 generations the sum is ${sumOf(20, state0)}")
  println(s"after 50000000000L generations the sum is ${sumOf(50000000000L, state0)}")

  def printState(state: Seq[State]) = {
    for {
      x <- state.head._2 to state.last._2
    } {
      print(if (x % 5 == 0) x else " ")
    }
    println()
    state.foreach{
      case (EmptyPot, _) => print(".")
      case (Plant, _) => print("#")
    }
    println()
  }

  def plant[_: P] = P("#").map(_ => Plant)
  def empty[_: P] = P(".").map(_ => EmptyPot)
  def pot[_: P]   = P(plant | empty)

  def initialState[_: P] = P("initial state: " ~ pot.rep)
  def rule[_: P] =
    P(pot.rep(exactly = 5) ~ " => " ~ pot).map(t => (BitSet(t._1.zipWithIndex.filter(_._1 == Plant).map(_._2):_*), t._2))
}
