package de.dheinrich

import fastparse.NoWhitespace._
import fastparse._

import scala.io.Source

class DayApp(day: Int, val useExample: Boolean = false, inputFile: String = "input") extends App {

  val file = if(useExample) "example" else inputFile

  val input = Source
    .fromResource(s"day$day/$file.txt")
    .getLines()
    .toIndexedSeq

  def parseInput[T](parser: P[_] => P[T]) = input.map(parseLine(parser, _))

  def parseLine[T](parser: P[_] => P[T], s: String) = {
    parse(s, parser).fold({
      case (msg, index, extra) =>
        throw new RuntimeException(extra.trace().longAggregateMsg)
    }, {
      case (t, i) => t
    })
  }

  def number[_: P] = P(("-".? ~ CharIn("0-9").rep(1)).!.map(_.toInt))
}
