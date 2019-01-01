package de.dheinrich

import de.dheinrich.TimeDevice.Instruction
import fastparse.JavaWhitespace._
import fastparse._

import scala.annotation.tailrec

object Day19 extends DayApp(19, inputFile = "optimized") {

  case class Input(a: Int, b: Int, c: Int) extends TimeDevice.Inputs

  def ip[_: P] = P("#ip" ~ number)
  def instruction[_: P]: P[Instruction] =
    P(CharIn("a-z").rep.!.map(TimeDevice.ops) ~ (number ~ number ~ number).map(Input.tupled))

  val ipointer     = parseLine(ip(_), input.head)
  val instructions = input.tail.map(s => parseLine(instruction(_), s))

  val result = TimeDevice.runProgram(Array(0, 0, 0, 0, 0, 0), ipointer)(instructions)
  println(s"first run ends with ${result(0)} in register 0")
  println(result)
//
//  val s = 976
  val s = 10551376

  @tailrec
  def calc(a: Int = 1, r: Int = 0): Int = {

    @tailrec
    def inner(r: Int, b: Int = a): Int = {
      val prod = a.toLong * b.toLong
      if(prod == s) {
        r + a + b
      } else if(prod > s)
        r
      else
        inner(r, b + 1)
    }

    if(a > s)
      r
    else {
      calc(a + 1, inner(r))
    }
  }

  val count = 0
  val r = calc()

  println(s"result is $r with $count of operations")
//
  val resultWith1 = TimeDevice.runProgram(Array(1, 0, 0, 0, 0, 0), ipointer)(instructions)
  println(resultWith1)
}
