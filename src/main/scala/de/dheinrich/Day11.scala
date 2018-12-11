package de.dheinrich
import de.dheinrich.Day11.GRID_SIZE

case class Grid(size: Int, data: Array[Int]) {
  def apply(x: Int, y: Int) = data(y * size + x)
  def update(x: Int, y: Int, value: Int) = {
    data(y * size + x) = value
  }

  def powerLevel(cell: FuelCell) =
    (for {
      y <- 0 until cell.size
      x <- 0 until cell.size
    } yield this(cell.x + x - 1, cell.y + y - 1)).sum

  def calcValuesFor(gridSerialNumber: Int) = {
    def calcPowerLevel(x: Int, y: Int) = {
      val rackId         = x + 10
      val powerLevelFull = (rackId * y + gridSerialNumber) * rackId
      (powerLevelFull / 100) % 10 - 5
    }

    for {
      x <- 1 to size
      y <- 1 to size
    } {
      this(x - 1, y - 1) = calcPowerLevel(x, y)
    }

    this
  }

  def maxFuelCellOfSize(s: Int) = {
    val cells = for {
      y <- 1 to (size - (s - 1))
      x <- 1 to (size - (s - 1))
    } yield FuelCell(x, y, s)

    cells.map(c => (c, powerLevel(c))).maxBy(_._2)
  }
}

object Grid {
  def apply(size: Int): Grid = Grid(size, Array.ofDim(size * size))
}

case class FuelCell(x: Int, y: Int, size: Int) {
  def minPowerLevel = size * size * -5
}

object Day11 extends App {

  val GRID_SERIAL_NUMBER = 9995
  val GRID_SIZE          = 300

  def calcMax3x3FuelCell(gridSerialNumber: Int) = {
    val grid = Grid(GRID_SIZE).calcValuesFor(gridSerialNumber)
    grid.maxFuelCellOfSize(3)
  }

  def calcMaxFuelCell(gridSerialNumber: Int) = {
    val grid       = Grid(GRID_SIZE).calcValuesFor(gridSerialNumber)
    val maxPerSize = Stream.from(1).take(GRID_SIZE).map(grid.maxFuelCellOfSize)
    maxPerSize.zip(maxPerSize.tail).takeWhile(t => t._1._2 < t._2._2).last._2
  }

  println(calcMax3x3FuelCell(18) == (FuelCell(33, 45, 3), 29))
  println(calcMax3x3FuelCell(42) == (FuelCell(21, 61, 3), 30))

  println(calcMaxFuelCell(18) == (FuelCell(90, 269, 16), 113))
  println(calcMaxFuelCell(42) == (FuelCell(232, 251, 12), 119))
  println(calcMaxFuelCell(GRID_SERIAL_NUMBER))
}
