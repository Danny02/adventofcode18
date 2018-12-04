package de.dheinrich

object Day2 extends DayApp(2) {

  val (twins, triplets) = input.map(hasTwinsOrTriplets).unzip

  val twinCount     = twins.filter(b => b).size
  val tripletsCount = triplets.filter(b => b).size

  val checksum = twinCount * tripletsCount

  println(s"the id checksum is $checksum")

  val sameChars = for {
    Seq(a, b) <- input.combinations(2)
    d = distance(a, b) if d == 1
    (ac, bc) <- a.zip(b) if ac == bc
  } yield ac

  println(s"the boxes share following id chars ${sameChars.mkString}")

  def hasTwinsOrTriplets(id: String) = {
    val grouped = id.groupBy(c => c).mapValues(_.length)
    (grouped.exists(_._2 == 2), grouped.exists(_._2 == 3))
  }

  def distance(a: String, b: String) =
    a.zip(b).filter(cc => cc._1 != cc._2).size
}
