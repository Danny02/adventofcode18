package de.dheinrich

import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import fastparse.NoWhitespace._
import fastparse._

sealed trait LogEntry
case class Shift(id: Int) extends LogEntry
object Awake              extends LogEntry
object Sleep              extends LogEntry

object Day4 extends DayApp(4) {

  implicit val dateTimeOrd = Ordering.by((_: LocalDateTime).toEpochSecond(ZoneOffset.UTC))

  val log = parseInput(logEntry(_)).sortBy(_._1)

  val groups = log
    .foldLeft(List[((LocalDateTime, Int), List[(LocalDateTime, LogEntry)])]()) {
      case (l, (t, Shift(id))) => ((t, id), List()) +: l
      case ((s, ll) :: l, e)   => (s, ll :+ e) +: l
    }
    .toMap

  val sleepTimes = groups
    .mapValues(_.grouped(2).map {
      case List((a, Sleep), (b, Awake)) => a.getMinute until b.getMinute
    }.toList)
    .toList
    .groupBy(_._1._2)
    .mapValues(_.flatMap(_._2))

  val biggestSleeper   = sleepTimes.maxBy(_._2.map(_.size).sum)
  val mostSleeptMinute = findMostSleeptMinute(biggestSleeper._2).get._1

  println(
    s"Elve #${biggestSleeper._1} sleept most at the $mostSleeptMinute th minute")
  println(s"code: ${biggestSleeper._1 * mostSleeptMinute}")

  val maxPerElve = for {
    (id, ranges)    <- sleepTimes
    (minute, times) <- findMostSleeptMinute(ranges)
  } yield (times, minute, id)

  val (_, min, maxElve) = maxPerElve.maxBy(_._1)

  println(s"Elve #$maxElve sleept most at the same minute ($min) ")
  println(s"code: ${maxElve * min}")

  def findMostSleeptMinute(sleeps: List[Range]) = {
    val perMin = sleeps.flatten.groupBy(identity).mapValues(_.size)
    if(perMin.isEmpty)
      None
    else
      Some(perMin.maxBy(_._2))
  }

  def date[_: P] =
    P("[" ~ number ~ "-" ~ number ~ "-" ~ number ~ " " ~ number ~ ":" ~ number ~ "]")
      .map {
        case (year, month, day, hour, minute) =>
          LocalDate.of(year, month, day).atTime(hour, minute)
      }

  def id[_: P]    = P("Guard #" ~ number).map(Shift(_))
  def sleep[_: P] = P("falls asleep").map(_ => Sleep)
  def awake[_: P] = P("wakes up").map(_ => Awake)

  def logEntry[_: P] = P(date ~ " " ~ (id | sleep | awake))
}
