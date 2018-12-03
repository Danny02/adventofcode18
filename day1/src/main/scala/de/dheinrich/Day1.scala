package de.dheinrich

import scala.collection.mutable
import scala.io.Source

object Day1 extends App {

  val changes = Source
    .fromResource("input.txt")
    .getLines()
    .map(Integer.parseInt)
    .toIndexedSeq

  val sum = changes.sum
  println(s"the sum of the frequencies is $sum")

  val changeStream = Stream.from(0).map(i => changes(i % changes.size))

  val known = mutable.HashSet[Int]()
  val firstDuplicate = changeStream.scanLeft(0)(_ + _).find(i => !known.add(i))

  println(s"found duplicate frequency $firstDuplicate")
}
