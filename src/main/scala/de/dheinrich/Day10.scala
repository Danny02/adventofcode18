package de.dheinrich

import fastparse.SingleLineWhitespace._
import fastparse._

/**
  * @author Daniel Heinrich
  * @since 10.12.2018
  */
case class Star(position: Vec2, velocity: Vec2) {
  def advance = Star(position + velocity, velocity)
}

object Day10 extends DayApp(10) {

  var stars = parseInput(star(_))

  val starStates: Stream[Seq[Star]] = stars #:: starStates.map(_.map(_.advance))

  val starsAndBounds = starStates.map(s => (s,  BBox.from(s.map(_.position)).area))

  // advance the stars until the minimum bounding box is reached
  private val starBoundsUntilAligned = starsAndBounds.tail.zip(starsAndBounds).takeWhile {
    case (current, last) => current._2 < last._2
  }
  //unpack the final sequence of stars
  val alignedStars = starBoundsUntilAligned.last._1._1

  printStars(alignedStars)

  println(s"it took ${starBoundsUntilAligned.size}s for the stars to align")

  private def printStars(stars: Seq[Star]) = {
    //unpack the positions of the aligned stars
    val alignedPositions = stars.map(_.position).toSet
    val alBB = BBox.from(alignedPositions)

    for {
      y <- alBB.min.y to alBB.max.y
      x <- alBB.min.x to alBB.max.x
      pos = Vec2(x,y)
    } {
      print(if(alignedPositions.contains(pos)) "#" else ".")
      if(x == alBB.max.x) println()
    }
  }

  def vec2[_: P] = P("<" ~ number ~ "," ~ number ~ ">").map(Vec2.tupled)
  def star[_: P] = P("position=" ~ vec2 ~ "velocity=" ~ vec2).map(Star.tupled)
}
