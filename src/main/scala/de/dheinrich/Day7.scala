package de.dheinrich

import fastparse.NoWhitespace._
import fastparse._

import scala.annotation.tailrec

case class Step(name: Char, dependencies: Seq[Char])

object Step {
  def from(deps: Seq[(Char, Char)]) = {
    val stepsWitchDepend = deps.map(_._2).distinct
    (deps
      .groupBy(_._2)
      .mapValues(_.map(_._1))
      .map(t => Step(t._1, t._2)) ++
      deps
        .filterNot(t => stepsWitchDepend.contains(t._1))
        .distinct
        .map(t => Step(t._1, Nil))).toList
  }
}

object Day7 extends DayApp(7, true) {

  val deps = parseInput(step(_))

  val steps = Step.from(deps)

  case class Work(step: Char, finisheTime: Int)
  case class State(freeWorker: Int, time: Int = 0, inWork: Set[Work] = Set())

  @tailrec
  def doWork(state: State, workTime: Char => Int): State = {
    import state._

    val freedWorker = inWork.filter(_.finisheTime == time).size
    val available = freedWorker + freeWorker

    val notStarted = steps.filterNot(s => inWork.map(_.step).contains(s.name))
    val finishedSteps = inWork.filter(_.finisheTime <= time).map(_.step)
    val allDepsSatisfied = notStarted
      .filter(s => s.dependencies.forall(finishedSteps.contains)).distinct.sortBy(_.name)

    val startOn = allDepsSatisfied
      .take(available)
      .map(s => Work(s.name, workTime(s.name) + time))

    val nextWork = inWork ++ startOn
    val inflight = nextWork.filterNot(w => finishedSteps.contains(w.step))

    if(startOn.isEmpty)
      if(inflight.isEmpty)
        State(available, inWork.map(_.finisheTime).max, inWork)
      else
        doWork(State(available, inflight.map(_.finisheTime).min, inWork), workTime)
    else {
      doWork(State(available - startOn.size, inflight.map(_.finisheTime).min, nextWork), workTime)
    }
  }


  val singleWorker = doWork(State(1), _ => 1)
  val codeWord = singleWorker.inWork.toList.sortBy(_.finisheTime).map(_.step).mkString
  println(s"The order to work on the steps alone are '$codeWord'")

  private val calcWorkTime: Char => Int = c => (if(useExample) 0 else 60) + (c - 'A') + 1
  val finalState = doWork(State(if(useExample) 2 else 5), calcWorkTime)
  val lastItemFinishedAt = finalState.inWork.map(_.finisheTime).max
  printState(finalState, calcWorkTime)
  println(s"All steps are completed after $lastItemFinishedAt s")

  def step[_: P] =
    P("Step " ~ SingleChar ~ " must be finished before step " ~ SingleChar)

  def printState(state: State, workTime: Char => Int) = {
    println(f"${"Second"}%-10s${"Worker 1"}%-10s${"Worker 2"}%-10s${"Done"}%-10s")
    var last = List[Char]()
    for(t <- 0 to finalState.time) {
      val finishedSteps = finalState.inWork.filter(_.finisheTime <= t).map(_.step)
      val inflight = finalState.inWork
        .filterNot(w => finishedSteps.contains(w.step))
        .filter(w => t >= w.finisheTime - workTime(w.step) && t < w.finisheTime)
        .map(_.step)
        .toList.sorted

      val belegung = inflight ++ Seq.fill(state.freeWorker - inflight.size)('.')
      if(belegung != last){
        print(f"$t%-10s")
        for(b <- belegung) print(f"$b%-10s")
        val aaa = finalState.inWork.filter(_.finisheTime <= t).toList.sortBy(_.finisheTime)
        println(f"${aaa.map(_.step).mkString}%-10s")
        last = belegung
      }
    }
  }
}
