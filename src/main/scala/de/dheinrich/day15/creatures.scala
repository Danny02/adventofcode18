package de.dheinrich.day15
import de.dheinrich.Vec2

sealed trait Team
object Goblin extends Team
object Elve   extends Team

case class Creature(pos: Vec2, team: Team, power: Int, life: Int = 200) {

  def move(to: Vec2) = copy(pos = to)

  def hitBy(creature: Creature) = copy(life = life - creature.power)

  def isAlive = life > 0

  override def toString = s"${team.getClass.getSimpleName.dropRight(1)}($life) at $pos"
}
