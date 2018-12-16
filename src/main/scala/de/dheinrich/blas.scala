package de.dheinrich

case class BBox(min: Vec2, max: Vec2) {
  def width  = max.x - min.x
  def height = max.y - min.y
  def area   = width.toLong * height.toLong
  def isOnBorder(p: Vec2) =
    (p.x == min.x || p.x == max.x) && (p.y == min.y || p.y == max.y)
}

object BBox {
  def from(points: Traversable[Vec2]) = {
    val first = BBox(points.head, points.head)
    points.tail.foldLeft(first) {
      case (bbox, point) => BBox(bbox.min.min(point), bbox.max.max(point))
    }
  }
}

case class Vec2(x: Int, y: Int) {
  def min(o: Vec2) = Vec2(x.min(o.x), y.min(o.y))
  def max(o: Vec2) = Vec2(x.max(o.x), y.max(o.y))

  def add(o: Vec2) = Vec2(x + o.x, y + o.y)
  def +(o: Vec2)   = add(o)

  def mdist(o: Vec2) = Math.abs(x - o.x) + Math.abs(y - o.y)

  def nearest(points: Seq[Vec2]): Option[Int] = {
    val sorted = points.map(mdist).zipWithIndex.sortBy(_._1)
    if(sorted(0)._1 == sorted(1)._1)
      None
    else
      Some(sorted(0)._2)
  }

  def +(v: (Int, Int)) = Vec2(x + v._1, y + v._2)
  def -(v: (Int, Int)) = Vec2(x - v._1, y - v._2)

  override def toString: String = s"($x, $y)"
}

object Vec2 {
  implicit val posOrdering: Ordering[Vec2] = Ordering.Tuple2[Int, Int].on(v => (v.y, v.x))
}
