package de.dheinrich
import fastparse.P

import scala.io.Source
import fastparse._

class DayApp(day: Int, useExample: Boolean = false) extends App {

  val file = if(useExample) "example" else "input"

  val input = Source
    .fromResource(s"day$day/$file.txt")
    .getLines()
    .toIndexedSeq

  def parseInput[T](parser: P[_] => P[T]) = input.map(s => parse(s, parser).get.value)
}
