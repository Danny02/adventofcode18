package de.dheinrich.day15
import de.dheinrich.Vec2

import scala.annotation.tailrec

object Game {

  def run(startWorld: World): (Game, World) = {

    @tailrec
    def playTurns(current: Game, world: World, last: World): (Game, World) = {
      import current._
      if(world.units.map(_.team).distinct.size == 1)
        (current, world)
      else if(waitingTurn.isEmpty) {
//        println(s"end of round $roundCount")
//        if(world.units.map(_.copy(life = 1)) != last.units.map(_.copy(life = 1)))
//          Day15.printWorld(world)
        playTurns(Game(world.units.map(_.pos).sorted, roundCount + 1), world, world)
      } else{
        val turned = current.takeTurn(world)
        val alive = turned.units.map(_.pos)
        val tail = waitingTurn.tail.filter(alive.contains)
        playTurns(Game(tail, roundCount), turned, last)
      }
    }

    playTurns(Game(startWorld.units.map(_.pos).sorted), startWorld, startWorld)
  }
}

case class Game(waitingTurn: Seq[Vec2], roundCount: Int = 0) {
  type Move   = Option[Vec2]
  type Attack = Option[Creature]

  def inRangeTargets(w: World, creature: Creature): Seq[Creature] = {
    w.nearbyCreatures(creature.pos).filter(_.team != creature.team)
  }
  def adjacentToEnemy(w: World, creature: Creature): Seq[Vec2] = {
    w.units.filter(_.team != creature.team).map(_.pos).flatMap(w.adjacentAndFreeTo)
  }
  def chooseTarget(targets: Seq[Creature]): Creature = targets.sortBy(e => (e.life, e.pos)).head

  def takeTurn(world: World): World = {
    val current = world.units.find(_.pos == waitingTurn.head).get
    val targets = inRangeTargets(world, current)
    if(!targets.isEmpty) {
      val enemy = chooseTarget(targets)
//      println(s"$current attacks $enemy")
      world.update(enemy, enemy.hitBy(current))
    } else {
      val reachable = adjacentToEnemy(world, current).flatMap(p => world.findPath(current.pos, p))
      if(reachable.isEmpty) {
        world
      } else {
        val sorted = reachable.sortBy(p => (p.size, p.last))

        val walked  = current.move(sorted.head.head)
        val upWorld = world.update(current, walked)
//        println(s"move $current to ${walked.pos}")

        val targetsAfterMove = inRangeTargets(upWorld, walked)
        if(targetsAfterMove.isEmpty) {
          upWorld
        } else {
          val enemy = chooseTarget(targetsAfterMove)
//          println(s"$current attacks $enemy")
          upWorld.update(enemy, enemy.hitBy(current))
        }
      }
    }
  }
}
