package de.dheinrich

import fastparse.NoWhitespace._
import fastparse._

object Day6 extends DayApp(6) {

  val coords = parseInput(coordinate(_))

  val bbox = BBox.from(coords)

  val coordsInBB = for {
    y <- bbox.min.y to bbox.max.y
    x <- bbox.min.x to bbox.max.x
  } yield Vec2(x,y)

  val coordOwnership = coordsInBB.map(c => c.nearest(coords).map(i => (c,i))).flatten

  val infiniteIndices = coordOwnership.filter(t => bbox.isOnBorder(t._1)).map(_._2).toSet

  val max = coordOwnership.filter(t => !infiniteIndices.contains(t._2))
    .groupBy(_._2).values.map(_.size).max

  println(s"if dangerouse the safest region has a size of $max")

  val maxDist = if(useExample) 32 else 10000

  val coordsInRange = coordsInBB.filter(c => coords.map(c.mdist).sum  < maxDist)
  println(s"if save, there are ${coordsInRange.size} coordinations in a save range")

  def coordinate[_:P] = P(number ~ ", " ~ number).map(t => Vec2(t._1, t._2))

}
