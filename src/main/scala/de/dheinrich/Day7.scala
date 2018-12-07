package de.dheinrich

import fastparse._
import NoWhitespace._

import scala.annotation.tailrec

object Day7 extends DayApp(7, true) {

  val deps = parseInput(step(_))

  @tailrec
  def buildResult(d: Seq[(Char, Char)], result: String = ""): String = {
    println(s"uncompleted deps are ${d.mkString(", ")}")
    val steps = d.flatMap(t => List(t._1, t._2)).distinct
    println(s"unfinished steps ${steps.mkString(", ")}")
    val notReady = d.map(_._2).distinct
    val ready    = steps.filterNot(notReady.contains).sorted.head
    println(s"$ready is ready")

    val uncompleted  = d.filterNot(t => ready == t._1)
    val concatResult = result + ready

    if(uncompleted.isEmpty)
      concatResult + notReady.head
    else
      buildResult(uncompleted, concatResult)
  }

  val result = buildResult(deps)
  println(s"result '$result' should be 'CABDFE' : ${result == "CABDFE"}")

  val workerCount = 2
  val minWorkTime = 0

  @tailrec
  def buildResultWithTime(d: Seq[(Char, Char)],
                          finished: Map[Char, Int] = Map()): String = {
    val steps = d.flatMap(t => List(t._1, t._2)).distinct

    val unfinished = steps.filterNot(finished.contains)
    println(s"need to work on ${unfinished.mkString(", ")}")

    val readyToStart = unfinished.filter(s => {
      val needs = d.filter(_._2 == s).map(_._1)
      needs.forall(finished.contains)
    })
    println(s"can start with ${readyToStart.mkString(", ")}")

    val finishedAt = readyToStart.map(s => {
      val needs   = d.filter(_._2 == s).map(_._1)
      val ownTime = calcWorkTime(s)
      val finishedAt =
        if(!needs.isEmpty)
          needs.map(finished).max + ownTime
        else
          ownTime
      (s, finishedAt)
    })
    println(s"steps should be finished at ${finishedAt.mkString(", ")}")

    if(finishedAt.isEmpty)
      (finished ++ finishedAt).toList.sortBy(_._2).map(_._1).mkString
    else
      buildResultWithTime(d, finished ++ finishedAt)
  }

  println(deps)
  val result2 = buildResultWithTime(deps)
  println(s"result '$result2' should be 'CABFDE' : ${result2 == "CABFDE"}")

  def calcWorkTime(step: Char) = minWorkTime + (step - 'A') + 1

  def step[_: P] =
    P("Step " ~ SingleChar ~ " must be finished before step " ~ SingleChar)
}
