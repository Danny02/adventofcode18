package de.dheinrich.day15
import de.dheinrich.Vec2

import scala.annotation.tailrec

object AStar extends App {

  println(Seq(Vec2(2,2), Vec2(1,1)).sorted)

  println(findPath(Vec2(1,1), Vec2(3,3), p => Seq(p + (0, -1), p + (1, 0), p + (0, 1), p + (-1, 0))))

  sealed trait Node {
    def cost: Int
    def pos: Vec2
    def prediction: Int
    lazy val total = cost + prediction

    override def toString: String = getClass.getSimpleName + s"(${pos.x}, ${pos.y})"
  }
  case class FirstNode(pos: Vec2, prediction: Int) extends Node {
    val cost = 1
  }
  case class WayNode(pos: Vec2, prediction: Int, parent: Node) extends Node {
    lazy val cost = parent.cost + 1
  }

  def findPath(from: Vec2, to: Vec2, adjacentTo: Vec2 => Seq[Vec2]): Option[Seq[Vec2]] = {
    def heuristic(from: Vec2, to: Vec2) = 0//(from.x - to.x).abs + (from.y - to.y).abs

    implicit val nodeOrdering = Ordering.by((n: Node) => (n.total, n.pos))

    val openNodes = adjacentTo(from).map(p => FirstNode(p, heuristic(p, to)))

    @tailrec
    def searchRecursive(open: Seq[Node], closed: Set[Vec2]): Option[Node] = {
      if(open.isEmpty)
        None
      else {
        val sorted = open.sorted
        val h      = sorted.head

        if(h.pos == to)
          Some(h)
        else {
          val nowClosed = closed + h.pos
          val notClosed = adjacentTo(h.pos).filterNot(nowClosed.contains).map(p => WayNode(p, heuristic(p, to), h))

          val withBetter = (sorted.tail ++ notClosed)
            .groupBy(_.pos)
            .map {
              case (_, l) => l.sorted.head
            }
            .toSeq

          searchRecursive(withBetter, nowClosed)
        }
      }
    }

    searchRecursive(openNodes, Set(from)).map(n => {
      @tailrec
      def buildPathRec(o: Node, path: Seq[Vec2] = Seq()): Seq[Vec2] = o match {
        case WayNode(pos, _, parent) => buildPathRec(parent, path :+ pos)
        case FirstNode(pos, _)       => (path :+ pos).reverse
      }

      buildPathRec(n)
    })
  }
}
