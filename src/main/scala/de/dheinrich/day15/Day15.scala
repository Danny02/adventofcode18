package de.dheinrich.day15
import de.dheinrich.{DayApp, Vec2}

object Day15 extends DayApp(15) {

  val world = parseWorld(input, 25)

  printWorld(world)

  val (finalTurn, finalState) = Game.run(world)

  printWorld(finalState)

  val hp    = finalState.units.map(_.life)
  val hpSum = hp.sum
  println(s"hitpoints: ${hp.mkString("+")} = $hpSum")
  println(s"${finalTurn.roundCount} * $hpSum = ${finalTurn.roundCount * hpSum}")

  def printWorld(world: World) = {
    val ul = world.units.map(u => (u.pos, u)).toMap
    for {
      y <- 1 to world.size.y
      x <- 1 to world.size.x
      pos  = Vec2(x, y)
      unit = ul.get(pos)
    } {
      if(unit.isDefined)
        print(if(unit.get.team == Goblin) 'G' else 'E')
      else if(world.walls.contains(pos))
        print('#')
      else
        print('.')

      if(x == world.size.x) {
        val unitsOfLine = world.units.filter(_.pos.y == y).sortBy(_.pos.x)
        val reps = unitsOfLine.map(u => u.toString.charAt(0) + s"(${u.life})")
        println(reps.mkString("\t", ", ", ""))
      }
    }
  }

  def printPath(path: Seq[Vec2], world: World) = {
    val start  = path.head
    val finish = path.last

    for {
      y <- 1 to world.size.y
      x <- 1 to world.size.x
      pos = Vec2(x, y)
    } {
      print(pos match {
        case x if x == start                => 'S'
        case x if x == finish               => 'F'
        case x if path.contains(x)          => "+"
        case x if world.blocked.contains(x) => "#"
        case _                              => '.'
      })
      if(x == world.size.x)
        println()
    }
  }

  def parseWorld(lines: Seq[String], elvePower: Int): World = {
    val parsed = for {
      (line, li)   <- input.zipWithIndex if li > 0 && li < input.size - 1
      (symbol, si) <- line.zipWithIndex if si > 0 && si < line.size - 1
      pos = Vec2(si, li)
    } yield
      symbol match {
        case '#' => (Some(pos), None)
        case '.' => (None, None)
        case 'G' => (Some(pos), Some(Goblin))
        case 'E' => (Some(pos), Some(Elve))
      }

    val size  = Vec2(input(0).size - 2, input.size - 2)
    val walls = parsed.filter(_._2.isEmpty).flatMap(_._1)
    val units = parsed.collect {
      case (Some(p), Some(t)) => Creature(p, t, if(t == Elve) elvePower else 3)
    }

    World(size, walls.toSet, units)
  }
}
