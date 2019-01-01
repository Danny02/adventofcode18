package de.dheinrich

import fastparse.JavaWhitespace._
import fastparse._

object Day21 extends DayApp(21) {

  case class Input(a: Int, b: Int, c: Int) extends TimeDevice.Inputs {
    override def toString: String = s"[$a, $b, $c]"
  }

  def ip[_: P] = P("#ip" ~ number)
  def instruction[_: P]: P[Option[(String, Input)]] =
    P(CharIn("a-z").rep(1).! ~ (number ~ number ~ number).map(Input.tupled)).?

  val ipointer     = parseLine(ip(_), input.head)

  val instructions = input.tail.flatMap(s => parseLine(instruction(_), s.trim))


  println(instructions)

  val prog = instructions.map(i => (TimeDevice.ops(i._1), i._2))
//  val result = TimeDevice.runProgram(Array(16457176, 0, 0, 0, 0, 0), ipointer)(prog)
  val result = TimeDevice.runProgram(Array(0, 0, 0, 0, 0, 0), ipointer)(prog)
}
