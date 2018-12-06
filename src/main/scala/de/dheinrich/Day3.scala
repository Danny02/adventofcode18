package de.dheinrich

import fastparse.NoWhitespace._
import fastparse._

object Day3 extends DayApp(3) {

  val parsed = parseInput(claim(_))

  val allUsedSquars = parsed.flatMap(claimToCoords)

  val usage = allUsedSquars.groupBy(identity).mapValues(_.size)

  val multiClaimed = usage.values.filter(_ > 1).size

  println(s"$multiClaimed sqin are claimed multiple times")

  val singleUse = for {
    c <- parsed
    coords = claimToCoords(c)
    if coords.map(usage).filter(_ != 1).isEmpty
  } yield c._1

  println(
    s"The claim with id #${singleUse.head} does not intersect with any other")

  def claimToCoords(c: Claim): Seq[(Int, Int)] = {
    for {
      x <- 0 until c._3._1
      y <- 0 until c._3._2
    } yield (x + c._2._1, y + c._2._2)
  }

  type Claim = (Int, (Int, Int), (Int, Int))

  def dimension[_: P] = P(number ~ "x" ~ number)

  def coord[_: P] = P(number ~ "," ~ number)

  def id[_: P] = P("#" ~ number)

  def claim[_: P]: P[Claim] = P(id ~ " @ " ~ coord ~ ": " ~ dimension)
}
