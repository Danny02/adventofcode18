package de.dheinrich
import scala.annotation.tailrec

object TimeDevice {

  trait Inputs {
    def a: Int;
    def b: Int;
    def c: Int;
  }

  type Instruction = (Operation, Inputs)

  type Memory = Array[Long]

  type Operation = (Memory, Inputs) => Memory

  private type SubOp = (Memory) => (Int, Int) => Long

  private def toOp(subOp: SubOp): Operation = (m, i) => {
    m(i.c) = subOp(m)(i.a, i.b)
    m
  }

  val ops = Map[String, Operation](
    //    Addition:
    "addr" -> toOp(m => m(_) + m(_)),
    "addi" -> toOp(m => m(_) + _),
    //    Multiplication:
    "mulr" -> toOp(m => m(_) * m(_)),
    "muli" -> toOp(m => m(_) * _),
    //    Bitwise AND:
    "banr" -> toOp(m => m(_) & m(_)),
    "bani" -> toOp(m => m(_) & _),
    //    Bitwise OR:
    "borr" -> toOp(m => m(_) | m(_)),
    "bori" -> toOp(m => m(_) | _),
    //      Assignment:
    "setr" -> toOp(m => (a, _) => m(a)),
    "seti" -> toOp(_ => (a, _) => a),
    //    Greater-than testing:
    "gtir" -> toOp(m => (a, b) => if(a > m(b)) 1 else 0),
    "gtri" -> toOp(m => (a, b) => if(m(a) > b) 1 else 0),
    "gtrr" -> toOp(m => (a, b) => if(m(a) > m(b)) 1 else 0),
    //    Equality testing:
    "eqir" -> toOp(m => (a, b) => if(a == m(b)) 1 else 0),
    "eqri" -> toOp(m => (a, b) => if(m(a) == b) 1 else 0),
    "eqrr" -> toOp(m => (a, b) => if(m(a) == m(b)) 1 else 0)
  )

  def runProgram(memory: Memory)(programm: IndexedSeq[Instruction]): Memory = {

    @tailrec
    def runLoop(memory: Memory, instrPointer: Int = 0): Memory = {
      if(instrPointer >= programm.size || instrPointer < 0)
        memory
      else {
        val instruction = programm(instrPointer)
        val applyed = instruction._1(memory, instruction._2)

        runLoop(applyed, instrPointer + 1)
      }
    }

    runLoop(memory)
  }

  def runProgram(memory: Memory, boundRegister: Int)(programm: IndexedSeq[Instruction]): Memory = {

    @tailrec
    def runLoop(memory: Memory, instrPointer: Int = 0,
                opsCounter: Long = 0, start: Long = System.currentTimeMillis()): Memory = {
      if(opsCounter % 1000000 == 0) {
        val runTime = System.currentTimeMillis() - start
        println(s"${opsCounter.toFloat / runTime} ops/ms, total $opsCounter")
      }

      if(instrPointer >= programm.size || instrPointer < 0)
        memory
      else {
        val instruction = programm(instrPointer)

        val injectIP = memory.updated(boundRegister, instrPointer.toLong)
        val applied = instruction._1(injectIP, instruction._2)

//        if(memory(0) != applied(0)) {
//          println(applied(1) + " " + applied(5))
//        }
//        if(memory(1) != applied(1)) {
//          println(applied(1) + " " + opsCounter)
//        }

        runLoop(applied, applied(boundRegister).toInt + 1, opsCounter + 1, start)
      }
    }

    runLoop(memory)
  }
}
