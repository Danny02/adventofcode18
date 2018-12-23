package de.dheinrich

import fastparse._
import SingleLineWhitespace._

import scala.annotation.tailrec

object Day16 extends DayApp(16) {

  case class Instruction(op: Int, a: Int, b: Int, c: Int)

  type State = Seq[Int]
  case class Example(before: State, instruction: Instruction, after: State)

  trait Operation {
    def name: String
    def apply(state: State, instruction: Instruction): State
  }

  def op(n: String)(f: (State, Instruction) => State) = new Operation {
    def name                                          = n
    def apply(state: State, instruction: Instruction) = f(state, instruction)
  }

  val ops = Seq(
    //    Addition:
    op("addr") {
      case (s, i) => s.updated(i.c, s(i.a) + s(i.b))
    },
    op("addi") {
      case (s, i) => s.updated(i.c, s(i.a) + i.b)
    },
    //    Multiplication:
    op("mulr") {
      case (s, i) => s.updated(i.c, s(i.a) * s(i.b))
    },
    op("muli") {
      case (s, i) => s.updated(i.c, s(i.a) * i.b)
    },
    //    Bitwise AND:
    op("banr") {
      case (s, i) => s.updated(i.c, s(i.a) & s(i.b))
    },
    op("bani") {
      case (s, i) => s.updated(i.c, s(i.a) & i.b)
    },
    //    Bitwise OR:
    op("borr") {
      case (s, i) => s.updated(i.c, s(i.a) | s(i.b))
    },
    op("bori") {
      case (s, i) => s.updated(i.c, s(i.a) | i.b)
    },
    //      Assignment:
    op("setr") {
      case (s, i) => s.updated(i.c, s(i.a))
    },
    op("seti") {
      case (s, i) => s.updated(i.c, i.a)
    },
    //    Greater-than testing:
    op("gtir") {
      case (s, i) => s.updated(i.c, if(i.a > s(i.b)) 1 else 0)
    },
    op("gtri") {
      case (s, i) => s.updated(i.c, if(s(i.a) > i.b) 1 else 0)
    },
    op("gtrr") {
      case (s, i) => s.updated(i.c, if(s(i.a) > s(i.b)) 1 else 0)
    },
    //    Equality testing:
    op("eqir") {
      case (s, i) => s.updated(i.c, if(i.a == s(i.b)) 1 else 0)
    },
    op("eqri") {
      case (s, i) => s.updated(i.c, if(s(i.a) == i.b) 1 else 0)
    },
    op("eqrr") {
      case (s, i) => s.updated(i.c, if(s(i.a) == s(i.b)) 1 else 0)
    },
  )

  def state[_: P]       = P("[" ~ number.rep(sep = ",", exactly = 4) ~ "]")
  def instruction[_: P] = P(number ~ number ~ number ~ number).map(Instruction.tupled)
  def example[_: P]     = P("Before:" ~ state ~ "\n" ~ instruction ~ "\n" ~ "After:" ~ state).map(Example.tupled)
  def exampleRep[_:P] = P(example.rep(sep = "\n\n"))

  def inputFile[_:P] = P(exampleRep ~ "\n\n\n\n" ~ instruction.rep(sep = "\n"))


  val (examples, programm) = parse(input.mkString("\n"), inputFile(_)).get.value

  val first = examples.head

  val canApply = ops.flatMap(o => {
    if(o(first.before, first.instruction) == first.after)
      Some(o.name)
    else
      None
  })

  println(canApply)

  val matchingOps = examples.map{ ex =>
    val matching = ops.filter(o => o(ex.before, ex.instruction) == ex.after)
    ex.instruction.op -> matching.map(_.name).toSet
  }

  val matchThreeOrMore = matchingOps.filter(_._2.size >= 3).size
  println(s"$matchThreeOrMore samples match 3 or more ops")

  val intersecting = matchingOps.groupBy(_._1).mapValues(t => t.map(_._2).reduce(_ intersect _))

  @tailrec
  def refine(open: Map[Int, Set[String]], closed: Map[Int, String] = Map()): Map[Int, String] = {
    val nc = closed ++ open.filter(_._2.size == 1).mapValues(_.head)
    val resolved = nc.values.toSet
    val no = open.filterKeys(k => !nc.contains(k)).mapValues(_.filterNot(resolved.contains))

    if(no.isEmpty)
      nc
    else
      refine(no, nc)
  }

  val refined = refine(intersecting).mapValues(n => ops.find(_.name == n).get)

  val result = programm.foldLeft(Seq(0,0,0,0)) {
    case (state, instruction) => refined(instruction.op)(state, instruction)
  }

  println(result)
}
