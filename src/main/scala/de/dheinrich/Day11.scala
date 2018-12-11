package de.dheinrich

/**
  * @author Daniel Heinrich
  * @since 11.12.2018
  */
object Day11 extends App {

  val GRID_SERIAL_NUMBER = 9995

  def calcMaxFuelCell(gridSerialNumber: Int) = {
    def calcPowerLevel(x: Int, y: Int) = {
      val rackId         = x + 10
      val powerLevelFull = (rackId * y + gridSerialNumber) * rackId
      (powerLevelFull / 100) % 10 - 5
    }

    val grid = Array.ofDim[Int](300, 300)
    for {
      x <- 1 to 300
      y <- 1 to 300
    } {
      grid(x - 1)(y - 1) = calcPowerLevel(x, y)
    }


    (for {
      s <- 1 to 300
      _ = println(s)
      y <- 1 to (300 - (s - 1))
      x <- 1 to (300 - (s - 1))
    } yield {
      val fuelCellLevel = (for {
        bx <- 0 until s
        by <- 0 until s
      } yield grid(x - 1 + bx)(y - 1 + by)).sum
      (x, y, s, fuelCellLevel)
    }).maxBy(_._4)
  }

//  println(calcMaxFuelCell(18) == (33,45,29))
//  println(calcMaxFuelCell(42) == (21,61,30))
//  println(calcMaxFuelCell(18) == (90,269,16,113))
//  println(calcMaxFuelCell(42) == (232,251,12,119))
  println(calcMaxFuelCell(GRID_SERIAL_NUMBER))
}
