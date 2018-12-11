package de.dheinrich
import org.scalatest.{FlatSpec, FunSuite}
import org.scalatest.prop.PropertyChecks

trait CircleBehavior {
  this: FlatSpec with PropertyChecks =>

  import Game._
  val game = Game(1, 1, None)

  trait Fac {
    def apply(initial: Int*): Circle
  }

  def normalCircle(circleFac: Fac) {
    it should "select should overflow" in {
      val circle = circleFac(0, 1, 2)
      assert(circle(3) == 0)
      assert(circle(4) == 1)
    }
    it should "select should handle all indices" in forAll {
      circleFac(0, 1, 2)(_)
    }
    it should "select should underflow" in {
      val circle = circleFac(1, 2, 3)
      assert(circle(-3) == 1)
    }
    it should "select should underflow correctly" in {
      val circle = circleFac(1, 2, 3, 4)
      assert(circle(-2) == 3)
    }

    it should "normalize should bring index in range" in forAll { i: Int =>
      val circle = circleFac(1, 2, 3, 4, 5)
      val ni = circle.normalizeIndex(i)
      assert(ni >= 0)
      assert(ni < circle.size)
    }
    it should "point to same marble with normalized index" in forAll { i: Int =>
      val circle = circleFac(1, 2, 3, 4, 5, 6)
      val ni = circle.normalizeIndex(i)
      assert(circle(i) == circle(ni))
    }

    it should "return correct size" in {
      val circle = circleFac(1, 2, 3, 4, 5, 6)
      assert(circle.size == 6)
    }

    it should "insert should shift remaining" in {
      val circle   = circleFac(0, 1, 2, 3, 4)
      val (at, nc) = circle.insertAfter(9, 2)
      assert(nc == circleFac(0, 1, 2, 9, 3, 4))
      assert(at == 3)
    }
    it should "insert should overflow" in {
      val circle   = circleFac(0, 1, 2)
      val (at, nc) = circle.insertAfter(9, 4)
      assert(nc == circleFac(0, 1, 9, 2))
      assert(at == 2)
    }
    it should "insert should add to size" in {
      val circle   = circleFac(0, 1, 2)
      val (_, nc) = circle.insertAfter(9, 4)
      assert(nc.size == 4)
    }

    it should "remove correct item" in {
      val circle = circleFac(0, 1, 2, 3)
      assert(circle.remove(2) == circleFac(0, 1, 3))
    }
    it should "remove should underflow" in {
      val circle = circleFac(0, 1, 2, 3)
      assert(circle.remove(-1) == circleFac(0, 1, 2))
    }
    it should "remove should decrease size" in {
      val circle = circleFac(0, 1, 2, 3)
      assert(circle.remove(-1).size == 3)
    }

    it should "should add to initial" in {
      assert(game.addMarble(State()).circle == circleFac(0, 1))
    }
    it should "should insert and increment next marble" in {
      val state = State(marble = 4)
      assert(game.addMarble(state).marble == 5)
    }
    it should "should insert at end" in {
      val state = State(currentIndex = 1, circle = circleFac(2, 3, 4))
      val ns    = game.addMarble(state)
      assert(ns.circle == circleFac(2, 3, 4, 1))
      assert(ns.currentIndex == 3)
    }
    it should "should insert with overflow" in {
      val state = State(currentIndex = 2, circle = circleFac(2, 3, 4))
      val ns    = game.addMarble(state)
      assert(ns.circle == circleFac(2, 1, 3, 4))
      assert(ns.currentIndex == 1)
    }

    it should "should remove and increment next marble" in {
      val state = State(marble = 5)
      assert(game.scoreNextMarble(state).marble == 6)
    }
    it should "should remove counter clockwise" in {
      val state =
        State(currentIndex = -2,
              circle = circleFac(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      val ns = game.scoreNextMarble(state)
      assert(ns.circle == circleFac(1, 3, 4, 5, 6, 7, 8, 9, 10))
      assert(ns.circle(ns.currentIndex) == 3)
    }
    it should "should remove set next index correctly for negative index" in {
      val state = State(currentIndex = -4, circle = circleFac(1, 2, 3))
      val ns    = game.scoreNextMarble(state)
      assert(ns.circle(ns.currentIndex) == 3)
    }
    it should "should remove set next index correctly for positive index" in {
      val state = State(currentIndex = 8, circle = circleFac(1, 2, 3))
      val ns    = game.scoreNextMarble(state)
      assert(ns.circle(ns.currentIndex) == 3)
    }

    it should "should score player" in {
      val state =
        State(currentIndex = 8,
              circle = circleFac(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      assert(game.scoreNextMarble(state).scores.contains(0))
    }
    it should "should score current player" in {
      val game2 = Game(3, 1, None)
      val state = State(currentIndex = 8, circle = circleFac(3))
      assert(game2.scoreNextMarble(state).scores.contains(1))
    }
    it should "should score by adding next marble & removed" in {
      val state =
        State(currentIndex = 1, marble = 17, circle = circleFac(1, 3, 6))
      assert(game.scoreNextMarble(state).scores(0) == 17 + 1)
    }
  }
}

class Day9Test extends FlatSpec with PropertyChecks with CircleBehavior {

  ("A VecCircle" should behave).like(normalCircle(s =>
    VecCircle(s.toIndexedSeq)))
  ("A MovingCircle" should behave).like(normalCircle(s =>
    MovingCircle(s.toIndexedSeq)))
}
