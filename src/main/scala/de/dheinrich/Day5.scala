package de.dheinrich
import scala.annotation.tailrec

object Day5 extends DayApp(5) {

  val polymer = input.head

  println(s"original polymer has ${polymer.size} chars")
  val reduced = reduce2(polymer)
  println(s"reduced to ${reduced.size} chars ($reduced)")

  val min = (for {
    remove <- 'a' to 'z'
    without = reduced.filter(_.toLower != remove)
  } yield reduce2(without).size).min

  println(s"the min size with one unit type removed is $min")

  @tailrec
  def reduce2(p: String): String = {
    val even = reduce(p)
    val odd = even.head +: reduce(even.tail)
    if(odd == p) p else reduce2(odd)
  }

  @tailrec
  def reduce(p: String): String = {
    val even = p.grouped(2).filter(s => s.size == 1 || s(0) == s(1) || s(0).toLower != s(1).toLower).mkString
    if(even == p) p else reduce(even)
  }
}
