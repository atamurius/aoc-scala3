package aoc2020

import common.Day

case object Day5 extends Day:
  
  case class Seat(row: Int, column: Int):
    def id = row * 8 + column
    override def toString: String = s"[row: $row, col: $column, id: $id]"
  
  def parseSeat(pass: String): Seat = {
    def navigate[T](xs: Iterable[T], left: T, right: T): (Int, Int) =
      val max = 1 << xs.size
      xs.foldLeft(0 -> max) {
        case ((l, r), d) =>
          val middle = (l + r) >> 1
          if (d == left) (l, middle)
          else (middle, r)
      }
    
    val (row, _) = navigate(pass.take(7), 'F', 'B')
    val (col, _) = navigate(pass.drop(7), 'L', 'R')
    Seat(row, col)
  }
  
  def findMissing(present: Set[Seat]): Set[Seat] =
    val idPresent = present.map(_.id)
    val seats = for 
      row <- 0 until 128
      col <- 0 until 8
      seat = Seat(row, col) 
      if !present(seat) && idPresent(seat.id + 1) && idPresent(seat.id - 1)
    yield seat
    seats.toSet
  

  override def test(): Unit =
    parseSeat("BFFFBBFRRR") shouldBe Seat(70, 7)
    parseSeat("FFFBBBFRRR") shouldBe Seat(14, 7)
    parseSeat("BFFFBBFRRR") shouldBe Seat(70, 7)
    parseSeat("BBFFBBFRLL") shouldBe Seat(102, 4)
  
  override def star1(): Any = readInput(_.map(parseSeat).maxBy(_.id))

  override def star2(): Any =
    val present = readInput(_.map(parseSeat).toSet)
    println()
    for (col <- 0 until 8) println(
      (0 until 128).iterator.map { row =>
        if present(Seat(row, col)) then 'X'
        else '.'
      }.mkString
    )
    findMissing(present) mkString ", "