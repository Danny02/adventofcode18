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

  case class Vec2(x: Int, y: Int) {
    def min(o: Vec2) = Vec2(x min o.x, y min o.y)
    def max(o: Vec2) = Vec2(x max o.x, y max o.y)

    def mdist(o: Vec2) = Math.abs(x - o.x) + Math.abs(y - o.y)

    def nearest(points: Seq[Vec2]): Option[Int] = {
      val sorted = points.map(mdist).zipWithIndex.sortBy(_._1)
      if (sorted(0)._1 == sorted(1)._1)
        None
      else
        Some(sorted(0)._2)
    }
  }

  case class BBox(min: Vec2, max: Vec2){
    def width = max.x - min.x
    def height = max.y - min.y
    def isOnBorder(p: Vec2) = (p.x == min.x || p.x == max.x) && (p.y == min.y || p.y == max.y)
  }

  object BBox {
    def from(points: Seq[Vec2]) = {
      val first = BBox(points.head, points.head)
      points.tail.foldLeft(first){
        case (bbox, point) => BBox(bbox.min min point, bbox.max max point)
      }
    }
  }
}
