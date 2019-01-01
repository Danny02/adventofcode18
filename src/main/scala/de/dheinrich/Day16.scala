package de.dheinrich

import de.dheinrich.TimeDevice.Inputs
import fastparse.SingleLineWhitespace._
import fastparse._

import scala.annotation.tailrec

object Day16 extends DayApp(16) {
  case class Instruction(op: Int, a: Int, b: Int, c: Int) extends Inputs

  case class Example(before: TimeDevice.Memory, instruction: Instruction, after: TimeDevice.Memory)

  def state[_: P]       = P("[" ~ number.map(_.toLong).rep(sep = ",", exactly = 4).map(_.toArray) ~ "]")
  def instruction[_: P] = P(number ~ number ~ number ~ number).map(Instruction.tupled)
  def example[_: P]     = P("Before:" ~ state ~ "\n" ~ instruction ~ "\n" ~ "After:" ~ state).map(Example.tupled)
  def exampleRep[_: P]  = P(example.rep(sep = "\n\n"))

  def inputFile[_: P] = P(exampleRep ~ "\n\n\n\n" ~ instruction.rep(sep = "\n"))

  val (examples, programm) = parse(input.mkString("\n"), inputFile(_)).get.value

  val matchingOps = examples.map { ex =>
    val matching = TimeDevice.ops.filter(o => o._2(ex.before, ex.instruction) == ex.after)
    ex.instruction.op -> matching.keySet
  }

  val matchThreeOrMore = matchingOps.filter(_._2.size >= 3).size
  println(s"$matchThreeOrMore samples match 3 or more ops")
  @tailrec
  def refine(open: Map[Int, Set[String]], closed: Map[Int, String] = Map()): Map[Int, String] = {
    val newlyResolved = open.filter(_._2.size == 1).mapValues(_.head)
    if(newlyResolved.isEmpty)
      throw new RuntimeException("should not happen")
    val nc       = closed ++ newlyResolved
    val resolved = nc.values.toSet
    val no = for {
      (k, v) <- open if !nc.contains(k)
    } yield k -> v.filterNot(resolved.contains)

    if(no.isEmpty)
      nc
    else
      refine(no, nc)
  }

  val intersecting = matchingOps.groupBy(_._1).mapValues(t => t.map(_._2).reduce(_.intersect(_)))
  val refined      = refine(intersecting).mapValues(n => TimeDevice.ops.find(_._1 == n).get._2)
  val runable      = programm.map(i => refined(i.op) -> i).toIndexedSeq

  val result = TimeDevice.runProgram(Array(0, 0, 0, 0))(runable)

  println(result)
}
