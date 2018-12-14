package de.dheinrich
import scala.annotation.tailrec

object Day14 extends App {

  case class State(recipes: IndexedSeq[Byte] = Vector(7, 3),
                   first: Int = 0,
                   second: Int = 1) {
    def size = recipes.size
  }

  def nextRecipes(state: State): State = {
    import state._

    val f = recipes(state.size - first - 1)
    val s = recipes(state.size - second - 1)

    val sum = f + s
    val added: IndexedSeq[Byte] =
      if(sum < 10)
        (sum % 10).toByte +: recipes
      else
         ((sum - 10) % 10).toByte +: 1.toByte +: recipes

    val size = added.size
    State(added, (first + f + 1) % size, (second + s + 1) % size)
  }

  // Part I
  @tailrec
  def afterN(n: Int, state: State = State()): IndexedSeq[Byte] = {
    val ns = nextRecipes(state)
    if(ns.size >= n + 10)
      ns.recipes.drop(ns.size - (n + 10)).take(10).reverse
    else
      afterN(n, ns)
  }

  def calcAfterN(n: Int) = println(s"After $n recipes, the scores of the next ten would be  ${afterN(n).mkString}")

  calcAfterN(5)
  println("\tshould be 0124515891")
  calcAfterN(9)
  println("\tshould be 5158916779")
  calcAfterN(18)
  println("\tshould be 9251071085")
  calcAfterN(2018)
  println("\tshould be 5941429882")

  // Part II
  @tailrec
  def beforeN(n: IndexedSeq[Byte], state: State = State()): Int = {
    val ns = nextRecipes(state)

    val atEnd = ns.recipes.startsWith(n)
    if(atEnd || ns.recipes.drop(1).startsWith(n)) {
      if(atEnd)
        ns.size - n.size
      else
        ns.size - n.size - 1
    } else{
      beforeN(n, ns)
    }
  }

  def split(i: String) = i.map(_.toString.toByte).reverse

  def calcBeforeN(n: String) = println(s"$n first appears after ${beforeN(split(n))} recipes")

  calcBeforeN("51589")
  println("\tshould be 9")
  calcBeforeN("01245")
  println("\tshould be 5")
  calcBeforeN("92510")
  println("\tshould be 18")
  calcBeforeN("59414")
  println("\tshould be 2018")

  calcBeforeN("430971")
}
