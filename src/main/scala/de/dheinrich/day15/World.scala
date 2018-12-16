package de.dheinrich.day15
import de.dheinrich.Vec2

case class World(size: Vec2, walls: Set[Vec2], units: Seq[Creature]) {
  private val cl = units.map(c => (c.pos, c)).toMap
  val blocked    = walls ++ cl.keySet

  def nearbyCreatures(p: Vec2) = adjacentTo(p).flatMap(cl.get)

  def adjacentTo(p: Vec2) = {
    Seq(p + (0, -1), p + (1, 0), p + (0, 1), p + (-1, 0))
  }

  def adjacentAndFreeTo(p: Vec2) = {
    adjacentTo(p)
      .filter(isFreeCell)
      .filterNot(p => p.x < 1 || p.y < 1 || p.x > size.x || p.y > size.y)
  }

  def isFreeCell(p: Vec2) = !blocked.contains(p)

  def findPath(from: Vec2, to: Vec2) = AStar.findPath(from, to, adjacentAndFreeTo)

  def update(old: Creature, next: Creature): World = {
    if(next.isAlive) {
      val i = units.indexWhere(_.pos == old.pos)
      World(size, walls, units.updated(i, next))
    } else {
      println(s"$old dies")
      World(size, walls, units.filterNot(_.pos == old.pos))
    }
  }
}
