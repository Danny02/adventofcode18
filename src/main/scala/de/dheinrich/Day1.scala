package de.dheinrich
import scala.collection.mutable

object Day1 extends DayApp(1) {

  val changes = input.map(_.toInt)

  val sum = changes.sum
  println(s"the sum of the frequencies is $sum")

  val changeStream = Stream.from(0).map(i => changes(i % changes.size))

  val known          = mutable.HashSet[Int]()
  val firstDuplicate = changeStream.scanLeft(0)(_ + _).find(i => !known.add(i))

  println(s"found duplicate frequency $firstDuplicate")
}
