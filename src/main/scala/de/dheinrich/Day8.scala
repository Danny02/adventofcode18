package de.dheinrich

import fastparse._
import SingleLineWhitespace._

case class Node(childs: Seq[Node], metaData: Seq[Int]) {
  def includedMeta: Seq[Int] = metaData ++ childs.flatMap(_.includedMeta)

  def value: Int =
    if(childs.isEmpty)
      metaData.sum
    else
      metaData
        .filter(i => 1 <= i && i <= childs.size)
        .map(i => childs(i - 1).value)
        .sum
}

object Day8 extends DayApp(8) {

  val root = parseInput(node(_)).head

  println(s"the sum of all metadata entries is ${root.includedMeta.sum}")
  println(s"the value of the root node is ${root.value}")

  def node[_: P]: P[Node] = P(
    for {
      (childCount, metaSize) <- header
      (children, metaData) <- nodeData(childCount, metaSize)
    } yield Node(children, metaData)
  )

  def nodeData[_: P](childCount: Int, metaSize: Int) =
    P(node.rep(exactly = childCount) ~ metadata(metaSize))

  def header[_: P] = P(number ~ number)

  def metadata[_: P](count: Int) = P(number.rep(exactly = count))

}
