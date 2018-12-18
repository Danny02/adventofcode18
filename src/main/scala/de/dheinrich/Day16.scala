package de.dheinrich

import fastparse._
import SingleLineWhitespace._

object Day16 extends DayApp(16) {

  val a =
    """
      |234,23,324
      |123,2
    """.stripMargin

  def ns[_:P] = P(number.rep(sep = ","))

  println(parse(a, ns(_)).get.value)
}
